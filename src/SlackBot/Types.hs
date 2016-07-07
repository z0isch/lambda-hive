{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module SlackBot.Types where

import           Control.Lens
import           Data.Map.Strict  (Map)
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T
import           LambdaHive.Types
import           Text.Regex.Posix
import           Web.Slack

data SlackBotGameState = WaitingForPlayerToJoin HivePlayer | Thinking | WaitingForPlayerToMove | GameOver
  deriving (Eq, Ord, Show)

data SlackBotCommand = StartHumanGame HivePlayer | StartAIGame HivePlayer UserId | MakeMove HiveMove | ShowGame | EndGame
  deriving (Eq, Show)

type GameStateKey = (Text,Text)
data SlackBotState = SlackBotState { _gameStates :: Map GameStateKey (SlackBotGameState,GameState) }
makeLenses ''SlackBotState

replyToUserText :: Submitter -> Text
replyToUserText (UserComment user)= "<@" <> user^.getId <> ">"
replyToUserText (BotComment bot) = "<@" <> bot^.getId <> ">"
replyToUserText _ = "System"

getGameStateKey :: ChannelId -> Submitter -> GameStateKey
getGameStateKey cid (UserComment user)= (cid^.getId, user^.getId)
getGameStateKey cid (BotComment bot) = (cid^.getId, bot^.getId)
getGameStateKey cid _ = (cid^.getId, "System")

messageMentionMe :: UserId -> Text -> Bool
messageMentionMe uId msg = T.unpack msg =~ mention
  where
    mention = T.unpack $ "<@" <> uId^.getId <> ">"

getUserWhoJoined :: Text -> UserId
getUserWhoJoined msg = Id $ T.pack $ head matches !! 1
  where r = T.unpack "<@(.*)\\|(.*)>"
        matches :: [[String]]
        matches =  T.unpack msg =~ r
