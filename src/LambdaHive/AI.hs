{-# LANGUAGE OverloadedStrings #-}

module LambdaHive.AI where


import           AI.Minimax
import qualified Data.Bimap                          as Bimap
import           Data.Bool
import qualified Data.Graph.Inductive.Graph          as Fgl
import           Data.Graph.Inductive.Query.ArtPoint
import           Data.List
import qualified Data.Map.Strict                     as Map
import           Data.Maybe
import           Data.Monoid
import           LambdaHive.Types
import           System.Random


data HiveAI = RandomAI | Minimax Int (GameState -> Value)

aiMove :: HiveAI -> GameState -> IO GameState
aiMove ai gs
   | null allMoves = return $ unsafeSkipTurn gs
   | otherwise = go ai
   where
      allMoves = validPlayerMoves gs
      go RandomAI = do
        r <- randomRIO (0, length allMoves -1)
        let mv = allMoves !! r
        putStrLn "Random Move"
        return $ fromJust $ makeMove gs mv
      go (Minimax d s) = do
        let (mv, score) = searchMove (alphaBeta s) d gs
        print score
        return $ fromJust $ makeMove gs mv

score1 :: GameState -> Value
score1 gs = case prog of
  Win wp -> if wp == p then maxBound else minBound
  Draw -> 0
  InProgress -> Value $
              sum artPiecePoints
              + (-100 * fromMaybe 0 opposingQueenBreathingRoom)
              + (10 * fromMaybe 0 (breathingRoom gs <$> queen p))
              + (1200 * enoughPiecesToWinVal)
  where
    p = gsCurrPlayer gs
    prog = gsStatus gs
    bs =  gsBoard gs
    oposingPlayer
      | p == Player1 = Player2
      | otherwise = Player1
    adjacency = bsAdjacency bs
    queen pl = (Bimap.!>) (bsIdMap bs) <$> maybeQueen pl
    maybeQueen pl = Bimap.lookup (playerText pl <> "Q") $ bsCannonicalIdMap bs
    artPoints = ap adjacency
    artPiecePoints = map ((\piece -> if hPlayer piece == p
                                     then -1 * pieceWeight (hPieceType piece)
                                     else pieceWeight $ hPieceType piece)
                         . getHivePieceFromId gs) artPoints
    pieceWeight Spider = 1
    pieceWeight Beetle = 3
    pieceWeight Grasshopper = 2
    pieceWeight Ant = 3
    pieceWeight Queen = 4
    opposingQueenBreathingRoom = breathingRoom gs <$> queen oposingPlayer
    enoughPiecesToWin = (>=) (piecesFreeToAttck gs p) <$> opposingQueenBreathingRoom
    enoughPiecesToWinVal = fromMaybe 0 $ bool (-1) 1 <$> enoughPiecesToWin

piecesFreeToAttck :: GameState -> HivePlayer -> Int
piecesFreeToAttck gs p = Map.size $ Map.filterWithKey freeToAttack coords
  where
    bs = gsBoard gs
    coords = bsCoords bs
    adjacency = bsAdjacency bs
    idMap = bsIdMap bs
    connMap = bsCannonicalIdMap bs
    queenText = playerText oposingPlayer <> "Q"
    queenExists = Bimap.member queenText connMap
    queenId =  idMap Bimap.!> (connMap Bimap.! queenText)
    adjacentToQueen = if queenExists
                      then Fgl.neighbors adjacency queenId
                      else []
    oposingPlayer
      | p == Player1 = Player2
      | otherwise = Player1
    freeToAttack pc pl = (hPlayer pl == p)
                       && (hPieceId pl `notElem` adjacentToQueen)
                       && not (null $ validPieceMoves gs pc)

breathingRoom :: GameState -> PieceId -> Int
breathingRoom gs = (-) 6
                  . genericLength
                  . nub
                  . filter (\(_,_,h) -> h == 0)
                  . map ((Bimap.!) (bsIdMap bs))
                  . Fgl.neighbors adjacency
  where
    bs = gsBoard gs
    adjacency = bsAdjacency bs
