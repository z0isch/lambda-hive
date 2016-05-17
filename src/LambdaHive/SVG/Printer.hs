{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module LambdaHive.SVG.Printer where

import           Control.Concurrent
import qualified Data.Map                as Map
import           Diagrams.Backend.Canvas
--import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import qualified Graphics.Blank          as B
import           LambdaHive.Types

hex :: String -> Diagram B
hex i = text i # fontSizeL 0.75 <> regPoly 6 1 # rotateBy (1/12)

hiveHex :: HivePiece -> Diagram B
hiveHex hp = text pieceText # fontSizeL 0.75 # fc (fontColor player) <> regPoly 6 1 # rotateBy (1/12) # fc (color player) # lc (color player)
  where
    pieceText = tail $ hCannonicalId hp
    color Player1 = oldlace
    color Player2 = black
    fontColor Player1 = black
    fontColor Player2 = oldlace
    player = hPlayer hp

possibleMoveHex :: Diagram B
possibleMoveHex = regPoly 6 1 # rotateBy (1/12) # lc green # lwN 0.01
moveFromHex :: Diagram B
moveFromHex = regPoly 6 1 # rotateBy (1/12) # lc red # lwN 0.01

coordToPoint :: PieceCoordinate -> P2 Double
coordToPoint (q,r,_) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    x = sqrt 3 * (r' + q'/2)
    y = 3/2 * q'

gameStateDiagram :: GameState -> Diagram B
gameStateDiagram gs = position currState
  where
    currState = map (\(pc,hp) -> (coordToPoint pc, hiveHex hp))
              $ Map.toList
              $ Map.filterWithKey (\pc _ -> topOfTheStack (gsBoard gs) pc)
              $ fst $ gsBoard gs

possibleMoves :: GameState -> PieceCoordinate -> Diagram B
possibleMoves gs pc
  | null moves = position [(coordToPoint pc,moveFromHex)]
  | otherwise = position $ (coordToPoint pc,moveFromHex) : map (\p -> (coordToPoint p , possibleMoveHex)) moves
  where moves = validPieceMoves gs pc

--testSVG :: IO ()
--testSVG = renderSVG "test.svg" (dims $ V2 300 300) (possibleMoves testGS5 (1,0,0) <> gameStateDiagram testGS5)

--mosquito :: IO (Either String (Diagram B))
--mosquito = fmap image <$> loadImageEmb "mosquito.png"

main :: IO ()
main = do
  canvas <- newMVar $ renderDia Canvas (CanvasOptions (dims $ V2 300 300)) $ gameStateDiagram testCircle9
  canvasThread <- forkIO $ B.blankCanvas 3000 $ canvasLoop canvas
  mainLoop testCircle9 canvas canvasThread

mainLoop :: GameState -> MVar (B.Canvas ()) -> ThreadId -> IO ()
mainLoop gs canvas canvasThread = do
  d <- getLine
  case d of
    "" -> killThread canvasThread
    _ -> do
      let foundPiece = getPieceCoord gs d
      case foundPiece of
        Just pc -> do
           _ <- swapMVar canvas $ renderDia Canvas (CanvasOptions (dims $ V2 300 300)) (possibleMoves gs pc <> gameStateDiagram gs)
           mainLoop gs canvas canvasThread
        Nothing -> mainLoop gs canvas canvasThread

canvasLoop :: MVar (B.Canvas ()) -> B.DeviceContext -> IO ()
canvasLoop canvas context = do
  c <- readMVar canvas
  B.send context $ do
    B.clearRect (0,0,B.width context,B.width context)
    c
  canvasLoop canvas context
