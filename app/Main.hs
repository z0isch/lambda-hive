module Main where

import           Control.Concurrent
import qualified Data.Graph.Inductive.Graph as Fgl
import           Data.Maybe
import           Diagrams.Backend.Canvas
import           Diagrams.Prelude
import qualified Graphics.Blank             as B
import           LambdaHive.AI
import           LambdaHive.Canvas
import           LambdaHive.Parser.Move
import           LambdaHive.Types
import qualified Text.Trifecta              as Tri

renderHiveCanvas :: (Monoid m, Semigroup m) => QDiagram Canvas V2 Double m -> B.Canvas ()
renderHiveCanvas = renderDia Canvas (CanvasOptions (dims $ V2 800 800))

main :: IO ()
main = do
  canvas <- newMVar $ renderHiveCanvas $ gameStateDiagram testGS6
  canvasThread <- forkIO $ B.blankCanvas 3000 $ canvasLoop canvas
  putStrLn $ Fgl.prettify $ bsAdjacency $ gsBoard testGS6
  --aiBattle initGS canvas canvasThread (Minimax 3 score1) RandomAI
  mainLoop testGS6 canvas canvasThread (Minimax 3 score1)

canvasLoop :: MVar (B.Canvas ()) -> B.DeviceContext -> IO ()
canvasLoop canvas context = do
  c <- readMVar canvas
  B.send context $ do
    B.clearRect (0,0,B.width context,B.width context)
    c
  canvasLoop canvas context

aiBattle :: GameState -> MVar (B.Canvas ()) -> ThreadId -> HiveAI -> HiveAI -> IO ()
aiBattle gs canvas canvasThread ai1 ai2 =
  case gsStatus gs of
    Win p -> showGameOver $ show p ++ " wins"
    Draw -> showGameOver "Draw"
    InProgress -> do
      newGs <- aiMove ai1 gs
      _ <- sendToCanvas $ gameStateDiagram newGs
      putStrLn $ Fgl.prettify $ bsAdjacency $ gsBoard gs
      threadDelay 300000
      case gsStatus newGs of
        Win p -> showGameOver $ show p ++ " wins"
        Draw -> showGameOver "Draw"
        InProgress -> do
          newGs2 <- aiMove ai2 newGs
          _ <- sendToCanvas $ gameStateDiagram newGs2
          putStrLn $ Fgl.prettify $ bsAdjacency $ gsBoard newGs2
          threadDelay 300000
          continueWith newGs2
  where
    continueWith g = aiBattle g canvas canvasThread ai1 ai2
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
        putStrLn $ Fgl.prettify $ bsAdjacency $ gsBoard g
        threadDelay 300000
        case gsStatus g of
          Win p -> showGameOver $ show p ++ " wins"
          Draw -> showGameOver "Draw"
          InProgress -> do
            newGs <- aiMove ai g
            _ <- sendToCanvas $ gameStateDiagram newGs
            putStrLn $ Fgl.prettify $ bsAdjacency $ gsBoard newGs
            continueWith newGs
      showGameOver s = do
        putStrLn s
        killThread canvasThread

makeTestMove :: GameState -> String -> GameState
makeTestMove gs s = fromJust $ makeMove gs sm
  where Tri.Success sm = Tri.parseString (moveParser <* Tri.eof) mempty s

testNextState :: GameState -> [GameState]
testNextState g = map (fromJust . makeMove g) $ validPlayerMoves g

testGS1 :: GameState
testGS1 = makeTestMove initGS "wQ ."
testGS2 :: GameState
testGS2 = makeTestMove testGS1 "bQ -wQ"
testGS3 :: GameState
testGS3 = makeTestMove testGS2 "wS1 wQ-"
testGS4 :: GameState
testGS4 = makeTestMove testGS3 "bB1 -bQ"
testGS5 :: GameState
testGS5 = makeTestMove testGS4 "wA1 wQ/"
testGS6 :: GameState
testGS6 = makeTestMove testGS5 "bB1 bQ"
testGS7 :: GameState
testGS7 = makeTestMove testGS6 "wA1 -bB1"

testCircle1 :: GameState
testCircle1 = unsafePlacePiece initGS (1,0,0) Queen
testCircle2 :: GameState
testCircle2 = unsafePlacePiece testCircle1 (-1,0,0) Queen
testCircle3 :: GameState
testCircle3 = unsafePlacePiece testCircle2 (0,-1,0) Spider
testCircle4 :: GameState
testCircle4 = unsafePlacePiece testCircle3 (-1,1,0) Grasshopper
testCircle5 :: GameState
testCircle5 = unsafePlacePiece testCircle4 (0,1,0) Ant
testCircle6 :: GameState
testCircle6 = unsafePlacePiece testCircle5 (1,0,1) Beetle
testCircle7 :: GameState
testCircle7 = unsafePlacePiece testCircle6 (1,-1,0) Ant
testCircle8 :: GameState
testCircle8 = unsafePlacePiece testCircle7 (1,-1,1) Beetle
testCircle9 :: GameState
testCircle9 = unsafePlacePiece testCircle8 (0,1,1) Beetle
