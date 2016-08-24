{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module LambdaHive.Diagram  where

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
    stackText = map (Text.unpack . hCannonicalId . snd)
              $ sortOn (\((_,_,h),_) -> -h)
              $ Map.toList $ Map.filterWithKey (\k _ -> axialEq pc k) bs
    color Player1 = lavender
    color Player2 = darkslategrey
    fontColor Player1 = darkslategrey
    fontColor Player2 = lavender
    player = hPlayer hp

possibleMoveHex = regPoly 6 1 # rotateBy (1/12) # lc green # lwN 0.005
moveFromHex = regPoly 6 1 # rotateBy (1/12) # lc red # lwN 0.005

coordToPoint :: PieceCoordinate -> P2 Double
coordToPoint (q,r,_) =  p2 (x,y)
  where
    q' = fromIntegral q
    r' = fromIntegral r
    x = sqrt 3 * (q' + (r'/2))
    y = -3/2 * r'

gameStateDiagram gs = position currState
  where
    currState = map (\(pc,_) -> (coordToPoint pc, hiveHex gs pc))
              $ Map.toList
              $ Map.filterWithKey (\pc _ -> topOfTheStack (gsBoard gs) pc)
              $ bsCoords $ gsBoard gs

possibleMoves gs pc
  | null moves = position [(coordToPoint pc,moveFromHex)]
  | otherwise = position $ (coordToPoint pc,moveFromHex) : map (\p -> (coordToPoint p , possibleMoveHex)) moves
  where moves = validPieceMoves gs pc
