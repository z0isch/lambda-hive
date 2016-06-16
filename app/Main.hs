module Main where

import           Control.Concurrent
import           Data.Maybe
import qualified Data.Text               as Text
import           Data.Time.Clock
import           Diagrams.Backend.Canvas
import           Diagrams.Prelude
import qualified Graphics.Blank          as B
import           LambdaHive.AI
import           LambdaHive.Canvas
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
sendToTestServer :: MVar (B.Canvas ()) -> GameState -> IO (B.Canvas ())
sendToTestServer canvas = swapMVar canvas . renderHiveCanvas . gameStateDiagram

main :: IO ()
main = do
  canvas <- newMVar $ renderHiveCanvas $ gameStateDiagram initGS
  t <- getCurrentTime
  gameLog <- openFile (show (utctDayTime t) ++ ".hive") WriteMode
  canvasThread <- forkIO $ B.blankCanvas 3000 $ canvasLoop canvas
  aiBattle gameLog initGS canvas canvasThread (Minimax 3 score1) RandomAI
  --mainLoop initGS canvas canvasThread (Minimax 3 score1)

canvasLoop :: MVar (B.Canvas ()) -> B.DeviceContext -> IO ()
canvasLoop canvas context = do
  c <- readMVar canvas
  B.send context $ do
    B.clearRect (0,0,B.width context,B.width context)
    c
  canvasLoop canvas context

aiBattle :: Handle -> GameState -> MVar (B.Canvas ()) -> ThreadId -> HiveAI -> HiveAI -> IO ()
aiBattle gH gs canvas canvasThread ai1 ai2 =
  case gsStatus gs of
    Win p -> showGameOver $ show p ++ " wins"
    Draw -> showGameOver "Draw"
    InProgress -> do
      (mv1,newGs) <- aiMove ai1 gs
      saveMove mv1
      _ <- sendToCanvas $ gameStateDiagram newGs
      case gsStatus newGs of
        Win p -> showGameOver $ show p ++ " wins"
        Draw -> showGameOver "Draw"
        InProgress -> do
          (mv2,newGs2) <- aiMove ai2 newGs
          saveMove mv2
          _ <- sendToCanvas $ gameStateDiagram newGs2
          continueWith newGs2
  where
    saveMove mv = do
      hPutStrLn gH $ Text.unpack $ getMoveString mv
      hFlush gH
    continueWith g = aiBattle gH g canvas canvasThread ai1 ai2
    sendToCanvas = swapMVar canvas . renderHiveCanvas
    showGameOver s = do
      putStrLn s
      killThread canvasThread

mainLoop :: GameState -> MVar (B.Canvas ()) -> ThreadId -> HiveAI -> IO ()
mainLoop gs canvas canvasThread ai = do
  d <- getLine
  case d of
    "end" -> showGameOver "Game Cancelled"
    _ -> do
      let piece = Tri.parseString (pieceParser <* Tri.eof) mempty d
      case piece of
        Tri.Success p -> showMoves p
        Tri.Failure _ -> do
          let move = Tri.parseString (moveParser <* Tri.eof) mempty d
          case move of
            Tri.Success sm -> do
              let mGs = makeMove gs sm
              case mGs of
                Just ngs -> do
                  print sm
                  playG ngs
                Nothing -> do
                    putStrLn "Enter a valid move or piece"
                    continueWith gs
            Tri.Failure _ -> do
                putStrLn "Enter a valid move or piece"
                continueWith gs
    where
      continueWith g = mainLoop g canvas canvasThread ai
      sendToCanvas = swapMVar canvas . renderHiveCanvas
      showMoves p =  do
        let foundPiece = getPieceCoord gs (getCannonicalId p)
        case foundPiece of
          Just pc -> do
             _ <- sendToCanvas $ possibleMoves gs pc <> gameStateDiagram gs
             continueWith gs
          Nothing -> continueWith gs
      playG g = do
        _ <- sendToCanvas $ gameStateDiagram g
        threadDelay 300000
        case gsStatus g of
          Win p -> showGameOver $ show p ++ " wins"
          Draw -> showGameOver "Draw"
          InProgress -> do
            (_,newGs) <- aiMove ai g
            _ <- sendToCanvas $ gameStateDiagram newGs ||| (text (scoreText newGs) # fontSizeL 0.25)
            continueWith newGs
      scoreText sGs = case ai of
        Minimax _ sAlg -> show $ sAlg sGs
        _ -> ""
      showGameOver s = do
        putStrLn s
        killThread canvasThread

makeTestMove :: GameState -> String -> GameState
makeTestMove gs s = fromJust $ makeMove gs sm
  where Tri.Success sm = Tri.parseString (moveParser <* Tri.eof) mempty s

testNextState :: GameState -> [GameState]
testNextState g = map (fromJust . makeMove g) $ validPlayerMoves g

getGameFromFile :: FilePath -> IO GameState
getGameFromFile f = foldl makeTestMove initGS . lines <$> readFile f
