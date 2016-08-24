module Main where

import           Control.Concurrent
import           Data.List.Zipper
import           Data.Maybe
import qualified Data.Text               as Text
import           Data.Time.Clock
import           Diagrams.Backend.Canvas
import           Diagrams.Prelude
import qualified Graphics.Blank          as B
import           LambdaHive.AI
import           LambdaHive.Diagram
import           LambdaHive.Parser.Move
import           LambdaHive.Types
import           System.IO
import qualified Text.Trifecta           as Tri

renderHiveCanvas :: (Monoid m, Semigroup m) => QDiagram Canvas V2 Double m -> B.Canvas ()
renderHiveCanvas = renderDia Canvas (CanvasOptions (dims $ V2 800 800))

startTestServer :: B.Options -> IO (MVar (B.Canvas ()), ThreadId)
startTestServer port = do
  canvas <- newMVar $ renderHiveCanvas $ gameStateDiagram initGS
  canvasThread <- forkIO $ B.blankCanvas port $ canvasLoop canvas
  return (canvas,canvasThread)
stopTestServer :: ThreadId -> IO ()
stopTestServer = killThread
sendToTestServer :: MVar (B.Canvas ()) -> GameState -> IO ()
sendToTestServer canvas = putMVar canvas . renderHiveCanvas . gameStateDiagram

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

main :: IO ()
main = do
  --game <- getGameFromFile "game2.hive"
  canvas <- newMVar $ renderHiveCanvas $ gameStateDiagram initGS
  canvasThread <- forkIO $ B.blankCanvas 3000 $ canvasLoop canvas
  --replayGame sw (fromList (tail game)) canvas canvasThread
  t <- getCurrentTime
  gameLog <- openFile (show (utctDayTime t) ++ ".hive") WriteMode
  -- aiBattle gameLog initGS canvas canvasThread
  --   (Minimax sw 4 score1)
  --   (Minimax sw 3 score1)
  playerVsAI gameLog initGS False canvas canvasThread (Minimax sw 4 score1)

canvasLoop :: MVar (B.Canvas ()) -> B.DeviceContext -> IO ()
canvasLoop canvas context = do
  c <- takeMVar canvas
  B.send context $ do
    B.clearRect (0,0,B.width context,B.width context)
    c
  canvasLoop canvas context

replayGame :: ScoreWeights -> Zipper GameState -> MVar (B.Canvas ()) -> ThreadId -> IO ()
replayGame sw gs canvas canvasThread = do
  print $ cursor gs
  _ <- putMVar canvas $ renderHiveCanvas gDia
  hSetBuffering stdin NoBuffering
  d <- getChar
  case d of
    'f' -> replayGame sw (right gs) canvas canvasThread
    'b' -> replayGame sw (left gs) canvas canvasThread
    _ -> replayGame sw gs canvas canvasThread
  where
    gDia = gameStateDiagram (cursor gs)

aiBattle :: Handle -> GameState -> MVar (B.Canvas ()) -> ThreadId -> HiveAI -> HiveAI -> IO ()
aiBattle gH gs canvas canvasThread ai1 ai2 =
  case gsStatus gs of
    Win p -> showGameOver $ show p ++ " wins"
    Draw -> showGameOver "Draw"
    InProgress -> do
      (mv1,newGs) <- aiMove ai1 gs
      saveMove newGs mv1
      _ <- sendToCanvas $ gameStateDiagram newGs
      threadDelay 300000
      case gsStatus newGs of
        Win p -> showGameOver $ show p ++ " wins"
        Draw -> showGameOver "Draw"
        InProgress -> do
          (mv2,newGs2) <- aiMove ai2 newGs
          saveMove newGs2 mv2
          _ <- sendToCanvas $ gameStateDiagram newGs2
          threadDelay 300000
          continueWith newGs2
  where
    saveMove ngs mv = do
      hPutStrLn gH $ Text.unpack $ getLegalMoveString ngs mv
      hFlush gH
    continueWith g = aiBattle gH g canvas canvasThread ai1 ai2
    sendToCanvas = putMVar canvas . renderHiveCanvas
    showGameOver s = do
      putStrLn s
      killThread canvasThread

playerVsAI :: Handle -> GameState -> Bool -> MVar (B.Canvas ()) -> ThreadId -> HiveAI -> IO ()
playerVsAI gH gs aiFirst canvas canvasThread ai =
  if aiFirst
  then go aiTurn playerTurn
  else go playerTurn aiTurn
  where
    go t1 t2 = do
      ngs <- t1 gs
      case ngs of
        Nothing -> return ()
        Just g -> do
          nngs <- t2 g
          case nngs of
            Nothing -> return ()
            Just g2 -> playerVsAI gH g2 aiFirst canvas canvasThread ai
    aiTurn iGs = do
      (mv,newGs) <- aiMove ai iGs
      _ <- sendToCanvas $ gameStateDiagram newGs
      _ <- saveMove newGs mv
      threadDelay 300000
      putStrLn $ Text.unpack $ getLegalMoveString newGs mv
      case gsStatus newGs of
        Win p -> showGameOver $ show p ++ " wins"
        Draw -> showGameOver "Draw"
        _ -> return $ Just newGs
    playerTurn iGs = do
      d <- getLine
      case d of
        "end" -> showGameOver "Game Cancelled"
        _ -> do
          let piece = Tri.parseString (pieceParser <* Tri.eof) mempty d
          case piece of
            Tri.Success p -> case getPieceCoord gs (getCannonicalId p) of
              Just pc -> do
                 _ <- sendToCanvas $ possibleMoves gs pc <> gameStateDiagram gs
                 playerTurn iGs
              Nothing -> playerTurn iGs
            Tri.Failure _ -> do
              let move = Tri.parseString (moveParser <* Tri.eof) mempty d
              case move of
                Tri.Success sm -> do
                  let mGs = makeMove iGs sm
                  case mGs of
                    Just ngs -> do
                      _ <- sendToCanvas $ gameStateDiagram ngs
                      _ <- saveMove ngs sm
                      threadDelay 300000
                      case gsStatus ngs of
                        Win p -> showGameOver $ show p ++ " wins"
                        Draw -> showGameOver "Draw"
                        _ -> return $ Just ngs
                    Nothing -> do
                        putStrLn "Enter a valid move or piece"
                        playerTurn iGs
                Tri.Failure _ -> do
                    putStrLn "Enter a valid move or piece"
                    playerTurn iGs
    sendToCanvas = putMVar canvas . renderHiveCanvas
    saveMove ngs mv = do
      hPutStrLn gH $ Text.unpack $ getLegalMoveString ngs mv
      hFlush gH
    showGameOver s = do
      putStrLn s
      killThread canvasThread
      return Nothing

makeTestMove :: GameState -> String -> GameState
makeTestMove gs s = fromJust $ makeMove gs sm
  where Tri.Success sm = Tri.parseString (moveParser <* Tri.eof) mempty s

testNextState :: GameState -> [GameState]
testNextState g = map (fromJust . makeMove g) $ validPlayerMoves g

getGameFromFile :: FilePath -> IO [GameState]
getGameFromFile f = scanl makeTestMove initGS . lines <$> readFile f
