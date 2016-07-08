{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module SlackBot.HiveBot where

import           Control.Lens
import           Control.Monad.State
import           Data.List
import qualified Data.Map.Strict        as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T
import           Data.UUID
import           Data.UUID.V4
import           Diagrams.Backend.Cairo
import qualified Diagrams.Prelude       as D
import           LambdaHive.AI
import           LambdaHive.Diagram
import           LambdaHive.Types
import           SlackBot.Parser
import           SlackBot.Types
import           SlackBot.WebAPI
import           System.Directory
import qualified Text.Trifecta          as Tri
import           Web.Slack
import           Web.Slack.Message

renderHivePNG :: GameState -> FilePath -> IO ()
renderHivePNG gs fp = renderCairo fp (D.dims $ D.V2 800 800) (gameStateDiagram gs)

sw :: ScoreWeights
sw = ScoreWeights
    { swPlayerArtPoints = 50
    , swOppArtPoints = 50
    , swOppQBreathing = 100
    , swPlayerQBreathing = 33
    , swPiecesToWin = 85
    , swPlayerPiecesOnTop = 85
    , swOppPiecesOnTop = 25
    }
botAI :: HiveAI
--botAI = Minimax sw 3 score1
botAI = RandomAI

postGameImage :: GameStateKey -> Slack SlackBotState ()
postGameImage gs@(cid,_) = do
  s <- get
  uid <- liftIO nextRandom
  let filename = toString uid ++ ".png"
      gameState = snd $ fromJust $ s^.userState.gameStates.at gs
  liftIO $ renderHivePNG gameState filename
  void $ liftIO $ postFileToSlack (_config s) cid filename
  liftIO $ removeFile filename

gameStart :: HivePlayer -> GameStateKey -> Slack SlackBotState ()
gameStart Player1 gs@(cid,submitter)= do
    (mv,ngs) <- liftIO $ aiMove botAI initGS
    userState.gameStates %= Map.alter (const $ Just (WaitingForPlayerToMove, ngs)) gs
    sendMessage (Id cid) $ "<@" <> submitter <> ">: " <> getMoveString mv
    postGameImage gs
gameStart Player2 gs@(cid,submitter) = do
  userState.gameStates %= Map.alter (const $ Just (WaitingForPlayerToMove, initGS)) gs
  sendMessage (Id cid) $ "<@" <> submitter <> ">: Your turn."

doSlackBotCommand :: GameStateKey -> SlackBotCommand -> Slack SlackBotState ()
doSlackBotCommand gs@(cid,_) (StartHumanGame hp) = do
  s <- get
  if Map.member gs (s^.userState.gameStates)
  then sendMessage (Id cid) "You already have a game going!"
  else gameStart hp gs
doSlackBotCommand (cid,submitter) (StartAIGame hp uId) = do
  s <- get
  let cs = s^.session.slackChannels
      c = fromJust $ find (\iC -> cid == iC^.channelId.getId) cs
      cMembers = fromMaybe [] $ c^.channelMembers
  if uId `elem` cMembers
  then gameStart hp (cid,uId^.getId)
  else do
    userState.gameStates %= Map.insert (cid,uId^.getId) (WaitingForPlayerToJoin hp, initGS)
    sendMessage (Id cid) $ "<@" <> submitter <> ">: Waiting for <@"<> uId^.getId <> "> to join."
doSlackBotCommand gs@(cid,submitter) (MakeMove hm) = do
  s <- get
  if Map.notMember gs (s^.userState.gameStates)
  then sendMessage (Id cid) "You have to start a game first!"
  else do
    let currGs = snd $ fromJust $ s^.userState.gameStates.at gs
        ngs = fromJust $ makeMove currGs hm
    userState.gameStates %= Map.update (const $ Just (Thinking, ngs)) gs
    postGameImage gs
    gameOverCheck ngs $ do
      (mv,aigs) <- liftIO $ aiMove botAI ngs
      userState.gameStates %= Map.update (const $ Just (WaitingForPlayerToMove, aigs)) gs
      sendMessage (Id cid) $ "<@" <> submitter <> ">: " <> getMoveString mv
      postGameImage gs
      gameOverCheck aigs $ return ()
    where
      gameOverCheck gamestate f
        | gameOver gamestate = do
          sendMessage (Id cid) $ "<@" <> submitter <> ">: " <> T.pack (show (gameStatus $ gsBoard gamestate))
          userState.gameStates %= Map.delete gs
          sendMessage (Id cid) $ "<@" <> submitter <> ">: Game over!"
        | otherwise   = f
doSlackBotCommand gs@(cid,submitter) EndGame =  do
  s <- get
  if Map.notMember gs (s^.userState.gameStates)
  then sendMessage (Id cid) "You have to start a game first!"
  else do
    userState.gameStates %= Map.delete gs
    sendMessage (Id cid) $ "<@" <> submitter <> ">: Game over!"
doSlackBotCommand gs@(cid,_) ShowGame =  do
  s <- get
  if Map.notMember gs (s^.userState.gameStates)
  then sendMessage (Id cid) "You have to start a game first!"
  else postGameImage gs

hiveBot :: SlackBot SlackBotState
hiveBot (Message cid _ msg _ (Just (SChannelJoin _)) _) = do
  s <- get
  let gs = (cid^.getId,getUserWhoJoined msg ^. getId)
  if Map.member gs (s^.userState.gameStates)
  then do
    let WaitingForPlayerToJoin hp = fst $ fromJust $ s^.userState.gameStates.at gs
    gameStart hp gs
  else sendMessage cid $ "<@" <> getUserWhoJoined msg^.getId <> ">: Would you like to play a game of Hive?"
hiveBot m@(Message cid submitter msg _ _ _) = do
  liftIO $ print m
  s <- get
  when (messageMentionMe (s^.session.slackSelf.selfUserId) msg) $ do
    let cmd = Tri.parseString (slackBotCommandParser <* Tri.eof) mempty (T.unpack msg)
    case cmd of
      Tri.Success sm -> doSlackBotCommand (getGameStateKey cid submitter) sm
      Tri.Failure _ -> sendMessage cid ("Sorry "<> replyToUserText submitter <>", I didn't understand that.")
hiveBot e = liftIO $ print e
