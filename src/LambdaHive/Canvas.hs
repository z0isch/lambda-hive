{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module LambdaHive.Canvas  where

import           Data.List
import qualified Data.Map         as Map
import qualified Data.Text        as Text
import           Diagrams.Prelude
import           LambdaHive.Types

hex = regPoly 6 1 # rotateBy (1/12)

hiveHex gs pc = text (intercalate ">" stackText) # fontSizeL (0.35 / genericLength stackText) # fc (fontColor player)
              <> hex # fc (color player) # lc black # lwN 0.005
  where
    bs = bsCoords $ gsBoard gs
    hp = (Map.!) bs pc
    stackText = map ((\p -> Text.unpack (hCannonicalId p) ++ "-" ++ show (hPieceId p))
                . snd)
              $ sortOn (\((_,_,h),_) -> -h)
              $ Map.toList $ Map.filterWithKey (\k _ -> axialEq pc k) bs
    color Player1 = lavender
    color Player2 = darkslategrey
    fontColor Player1 = darkslategrey
    fontColor Player2 = lavender
    player = hPlayer hp

--possibleMoveHex :: Diagram B
possibleMoveHex = regPoly 6 1 # rotateBy (1/12) # lc green # lwN 0.005
--moveFromHex :: Diagram B
moveFromHex = regPoly 6 1 # rotateBy (1/12) # lc red # lwN 0.005

coordToPoint :: PieceCoordinate -> P2 Double
coordToPoint (q,r,_) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    x = sqrt 3 * (q' + (r'/2))
    y = -3/2 * r'

--gameStateDiagram :: GameState -> Diagram B
gameStateDiagram gs = position currState
  where
    currState = map (\(pc,_) -> (coordToPoint pc, hiveHex gs pc))
              $ Map.toList
              $ Map.filterWithKey (\pc _ -> topOfTheStack (gsBoard gs) pc)
              $ bsCoords $ gsBoard gs

--possibleMoves :: GameState -> PieceCoordinate -> Diagram B
possibleMoves gs pc
  | null moves = position [(coordToPoint pc,moveFromHex)]
  | otherwise = position $ (coordToPoint pc,moveFromHex) : map (\p -> (coordToPoint p , possibleMoveHex)) moves
  where moves = validPieceMoves gs pc
