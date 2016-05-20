module Main where

import           Control.Concurrent
import           Data.Maybe
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
  canvas <- newMVar $ renderHiveCanvas $ gameStateDiagram initGS
  canvasThread <- forkIO $ B.blankCanvas 3000 $ canvasLoop canvas
  mainLoop initGS canvas canvasThread (Minimax 3)

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
    "" -> gameOver "Game Cancelled"
    _ -> do
      let piece = Tri.parseString (pieceParser <* Tri.eof) mempty d
      case piece of
        Tri.Success p -> showMoves p
        Tri.Failure _ -> do
          let move = Tri.parseString (moveParser <* Tri.eof) mempty d
          case move of
            Tri.Success sm -> makeMove sm
            Tri.Failure _ -> do
                putStrLn "Enter a valid move or piece"
                continueWith gs
    where
      continueWith g = mainLoop g canvas canvasThread ai
      sendToCanvas = swapMVar canvas . renderHiveCanvas
      showMoves p =  do
        let foundPiece = getPieceCoord gs (getCanonicalId p)
        case foundPiece of
          Just pc -> do
             _ <- sendToCanvas $ possibleMoves gs pc <> gameStateDiagram gs
             continueWith gs
          Nothing -> continueWith gs
      makeMove (SlideMove piece1@(PieceMove _ pm1) piece2 dir) = do
        let foundPiece1 = getPieceCoord gs (getCanonicalId piece1)
        let foundPiece2 = getPieceCoord gs (getCanonicalId piece2)
        let moveCoord = flip getNeighbor dir <$> foundPiece2
        case foundPiece1 of
          Just fp1 -> if isJust foundPiece1 && isJust foundPiece2
            then playG $ unsafeMovePiece gs fp1 (fromJust moveCoord)
            else continueWith gs
          Nothing -> if isJust foundPiece2
            then case pm1 of
              QueenMove -> playG $ unsafePlacePiece gs (fromJust moveCoord) Queen
              PieceMoveType pt1 _ -> playG $ unsafePlacePiece gs (fromJust moveCoord) pt1
            else continueWith gs
      makeMove (TopMove piece1 piece2) = do
        let foundPiece1 = getPieceCoord gs (getCanonicalId piece1)
        let foundPiece2 = getPieceCoord gs (getCanonicalId piece2)
        if isJust foundPiece1 && isJust foundPiece2
        then do
            let fp2@(x2,y2,_) = fromJust foundPiece2
            playG $ unsafeMovePiece gs (fromJust foundPiece1) (x2,y2,stackHeight' gs fp2)
        else continueWith gs
      makeMove (FirstMove (PieceMove _ QueenMove)) = playG $ unsafePlacePiece gs (0,0,0) Queen
      makeMove (FirstMove (PieceMove _ (PieceMoveType pT _))) = playG $ unsafePlacePiece gs (0,0,0) pT
      playG g = do
        _ <- sendToCanvas $ gameStateDiagram g
        threadDelay 350000
        case gsStatus g of
          Win p -> gameOver $ show p ++ " wins"
          Draw -> gameOver "Draw"
          InProgress -> do
            newGs <- aiMove ai g
            _ <- sendToCanvas $ gameStateDiagram newGs
            continueWith newGs
      gameOver s = do
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
