{-# LANGUAGE OverloadedStrings #-}

module LambdaHive.AI where

import           Control.Parallel.Strategies
import qualified Data.Bimap                          as Bimap
import qualified Data.Graph.Inductive.Graph          as Fgl
import           Data.Graph.Inductive.Query.ArtPoint
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           LambdaHive.Types
import           System.Random

data GameTree a = Node a [GameTree a]
  deriving (Eq,Show)

data HiveAI = RandomAI | Minimax Int ScoreAlgorithm

type ScoreAlgorithm = GameState -> Integer

aiMove :: HiveAI -> GameState -> IO GameState
aiMove ai gs
   | null allMoves = return $ unsafeSkipTurn gs
   | otherwise = go ai
   where
      allMoves = validPlayerMoves gs
      go RandomAI = do
        r <- randomRIO (0, length allMoves -1)
        let mv = allMoves !! r
        return $ fromJust $ makeMove gs mv
      go (Minimax i s) = do
        let (Node _ ts) = prune i $ buildTree s gs
        let stateScores = map (\t -> (minimax False t,t)) ts
        let bestScore = fst $ head $ sortOn fst stateScores
        let bestStates = filter ((==) bestScore . fst) stateScores
        r <- randomRIO (0, length bestStates - 1)
        let (Node mv _) = snd $ bestStates !! r
        return $ fromJust $ genGameState gs $ map fst mv

buildTree :: ScoreAlgorithm -> GameState -> GameTree [(HiveMove, Integer)]
buildTree s gs = go [(NoOp, s gs)]
 where
   go :: [(HiveMove, Integer)] -> GameTree [(HiveMove, Integer)]
   go hms = Node hms (map go cGs)
     where cGs = map (\nmv -> hms ++ [(nmv, s $ fromJust $ makeMove currState nmv)]) $ validPlayerMoves currState
           currState = fromJust $ genGameState gs (map fst hms)

prune :: Int -> GameTree a -> GameTree a
prune 0 (Node t _) = Node t []
prune n (Node t ts) = Node t $ map (prune (n-1)) ts

minimax :: Bool -> GameTree [(HiveMove, Integer)] -> Integer
minimax _ (Node hms []) = snd $ last hms
minimax True (Node _ ts) = maximum $ parMap rseq (minimax False) ts
minimax False (Node _ ts) = minimum $ parMap rseq (minimax True) ts

score1 :: ScoreAlgorithm
score1 gs = case prog of
  Win wp -> if wp == p then 1000 else -1000
  Draw -> -100
  InProgress -> sum artPiecePoints
              + (10 * fromMaybe 0 (breathingRoom <$> queen oposingPlayer))
              + (-2 * fromMaybe 0 (breathingRoom <$> queen p))
  where
    p = gsCurrPlayer gs
    prog = gsStatus gs
    bs =  gsBoard gs
    oposingPlayer
      | p == Player1 = Player2
      | otherwise = Player1
    adjacency = bsAdjacency bs
    queen pl = (Bimap.!>) (bsIdMap bs) <$> (Bimap.lookup (playerText pl <> "Q") $ bsCannonicalIdMap bs :: Maybe PieceCoordinate)
    breathingRoom = genericLength . filter (\(_,_,h) -> h ==0) . map ((Bimap.!) (bsIdMap bs)) . Fgl.neighbors adjacency
    artPoints = ap adjacency
    artPiecePoints = map ((\piece -> if hPlayer piece == p
                                     then -1 * pieceWeight (hPieceType piece)
                                     else pieceWeight $ hPieceType piece)
                         . getHivePieceFromId gs) artPoints
    pieceWeight Spider = 1
    pieceWeight Beetle = 3
    pieceWeight Grasshopper = 2
    pieceWeight Ant = 3
    pieceWeight Queen = 3
