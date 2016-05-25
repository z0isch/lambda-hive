module Main where

import           Control.Concurrent
import           Diagrams.Backend.Canvas
import           Diagrams.Prelude
import qualified Graphics.Blank          as B
import           LambdaHive.AI
import           LambdaHive.Canvas
import           LambdaHive.Parser.Move
import           LambdaHive.Types
import qualified Text.Trifecta           as Tri

renderHiveCanvas :: (Monoid m, Semigroup m) => QDiagram Canvas V2 Double m -> B.Canvas ()
renderHiveCanvas = renderDia Canvas (CanvasOptions (dims $ V2 600 600))

main :: IO ()
main = do
  canvas <- newMVar $ renderHiveCanvas $ gameStateDiagram testGS6
  canvasThread <- forkIO $ B.blankCanvas 3000 $ canvasLoop canvas
  mainLoop testGS6 canvas canvasThread (Minimax 2 score1)

canvasLoop :: MVar (B.Canvas ()) -> B.DeviceContext -> IO ()
canvasLoop canvas context = do
  c <- readMVar canvas
  B.send context $ do
    B.clearRect (0,0,B.width context,B.width context)
    c
  canvasLoop canvas context

mainLoop :: GameState -> MVar (B.Canvas ()) -> ThreadId -> HiveAI  -> IO ()
mainLoop gs canvas canvasThread ai = do
  d <- getLine
  case d of
    "" -> showGameOver "Game Cancelled"
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
                Just ngs -> playG ngs
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
        case gsStatus g of
          Win p -> showGameOver $ show p ++ " wins"
          Draw -> showGameOver "Draw"
          InProgress -> do
            newGs <- aiMove ai g
            _ <- sendToCanvas $ gameStateDiagram newGs
            continueWith newGs
      showGameOver s = do
        putStrLn s
        killThread canvasThread

testGS1 :: GameState
testGS1 = unsafePlacePiece initGS (0,0,0) Queen
testGS2 :: GameState
testGS2 = unsafePlacePiece testGS1 (-1,0,0) Queen
testGS3 :: GameState
testGS3 = unsafePlacePiece testGS2 (1,0,0) Spider
testGS4 :: GameState
testGS4 = unsafePlacePiece testGS3 (-2,0,0) Beetle
testGS5 :: GameState
testGS5 = unsafePlacePiece testGS4 (1,-1,0) Ant
testGS6 :: GameState
testGS6 = unsafeMovePiece testGS5 (-2,0,0) (-1,0,1)

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
