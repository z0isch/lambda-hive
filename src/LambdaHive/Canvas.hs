{-# LANGUAGE NoMonomorphismRestriction #-}

module LambdaHive.Canvas  where

import           Data.List
import qualified Data.Map                as Map
import           Diagrams.Backend.Canvas
import           Diagrams.Prelude
import           LambdaHive.Types

hex :: Diagram B
hex = regPoly 6 1 # rotateBy (1/12)

hiveHex :: GameState -> PieceCoordinate -> Diagram B
hiveHex gs pc = text (intercalate ">" stackText) # fontSizeL (0.5 / genericLength stackText) # fc (fontColor player)
              <> hex # fc (color player) # lc black # lwN 0.01
  where
    bs = fst $ gsBoard gs
    hp = (Map.!) bs pc
    stackText = map (hCannonicalId . snd)
              $ sortOn (\((_,_,h),_) -> -h)
              $ Map.toList $ Map.filterWithKey (\k _ -> axialEq pc k) bs
    color Player1 = lavender
    color Player2 = darkslategrey
    fontColor Player1 = darkslategrey
    fontColor Player2 = lavender
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
    x = sqrt 3 * (q' + (r'/2))
    y = -3/2 * r'

gameStateDiagram :: GameState -> Diagram B
gameStateDiagram gs = position currState
  where
    currState = map (\(pc,_) -> (coordToPoint pc, hiveHex gs pc))
              $ Map.toList
              $ Map.filterWithKey (\pc _ -> topOfTheStack (gsBoard gs) pc)
              $ fst $ gsBoard gs

possibleMoves :: GameState -> PieceCoordinate -> Diagram B
possibleMoves gs pc
  | null moves = position [(coordToPoint pc,moveFromHex)]
  | otherwise = position $ (coordToPoint pc,moveFromHex) : map (\p -> (coordToPoint p , possibleMoveHex)) moves
  where moves = validPieceMoves gs pc
