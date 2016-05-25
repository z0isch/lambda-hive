module LambdaHive.AI where

import           Control.Parallel.Strategies
import           Data.Graph.Inductive.Query.ArtPoint
import           Data.List
import           Data.Maybe
import           LambdaHive.Types
import           System.Random

data GameTree a = GameTree a [GameTree a]
  deriving (Eq,Show)

data HiveAI = RandomAI | Minimax Int ScoreAlgorithm

type ScoreAlgorithm = HivePlayer -> GameState -> Integer

aiMove :: HiveAI -> GameState -> IO GameState
aiMove ai gs
  | null allMoves = return $ unsafeSkipTurn gs
  | otherwise = go ai
  where
    allMoves = validPlayerMoves gs
    go RandomAI = do
      r <- randomRIO (0, length allMoves -1)
      let mv = allMoves !! r
      return $ fromJust $ genGameState gs [mv]
    go (Minimax i s) = do
      let (GameTree _ ts) = prune i $ buildTree s gs
      let stateScores = map (\t -> (minimax False t,t)) ts
      let bestScore = fst $ head $ sortOn fst stateScores
      let bestStates = filter ((==) bestScore . fst) stateScores
      r <- randomRIO (0, length bestStates - 1)
      let (GameTree mv _) = snd $ bestStates !! r
      return $ fromJust $ genGameState gs $ map fst mv

buildTree :: ScoreAlgorithm -> GameState -> GameTree [(HiveMove, Integer)]
buildTree s gs = go [(NoOp, s currPlayer gs)]
  where
    currPlayer = gsCurrPlayer gs
    go :: [(HiveMove, Integer)] -> GameTree [(HiveMove, Integer)]
    go hms = GameTree hms (map go cGs)
      where cGs = map (\nmv -> hms ++ [(nmv, s currPlayer $ fromJust $ makeMove currState nmv)]) $ validPlayerMoves currState
            currState = fromJust $ genGameState gs (map fst hms)

prune :: Int -> GameTree a -> GameTree a
prune level = go 0
  where
    go i (GameTree b ts)
      | i < level = GameTree b (map (go (i+1)) ts)
      | otherwise = GameTree b []

minimax :: Bool -> GameTree [(HiveMove, Integer)] -> Integer
minimax _ (GameTree hms []) = snd $ last hms
minimax True (GameTree _ ts) = maximum $ parMap rseq (minimax False) ts
minimax False (GameTree _ ts) = minimum $ parMap rseq (minimax True) ts

score1 :: ScoreAlgorithm
score1 p gs = case prog of
  Win wp -> if wp == p then 1000 else -1000
  Draw -> -100
  InProgress -> sum artPiecePoints
  where
    prog = gsStatus gs
    bs = gsBoard gs
    artPoints = ap $ bsAdjacency bs
    artPiecePoints = map ((\piece -> if hPlayer piece == p then -1 else 1) . getHivePieceFromId gs) artPoints
