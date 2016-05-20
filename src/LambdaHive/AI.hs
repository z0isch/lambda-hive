module LambdaHive.AI where

import           Control.Parallel.Strategies
import           Data.List
import           LambdaHive.Types
import           System.Random

data GameTree = GameTree GameState [GameTree]
  deriving (Eq,Ord,Show)

data HiveAI = RandomAI | Minimax Int
  deriving (Eq,Ord,Show)

aiMove :: HiveAI -> GameState -> IO GameState
aiMove ai gs
  | null allMoves = return $ unsafeSkipTurn gs
  | otherwise = go ai
  where
    allMoves = validPlayerMoves gs
    go RandomAI = do
      r <- randomRIO (0, length allMoves -1)
      return (allMoves !! r)
    go (Minimax i) = do
      let (GameTree _ ts) = prune i $ buildTree gs
      let stateScores = map (\t -> (minimax score1 (gsCurrPlayer gs) t,t)) ts
      let bestScore = fst $ head $ sortOn fst stateScores
      let bestStates = filter ((==) bestScore . fst) stateScores
      r <- randomRIO (0, length bestStates - 1)
      let (GameTree ngs _) = snd $ bestStates !! r
      return ngs

buildTree :: GameState -> GameTree
buildTree gs = GameTree gs $ map buildTree $ validPlayerMoves gs

prune :: Int -> GameTree -> GameTree
prune level = go 0
  where
    go i (GameTree gs ts)
      | i < level = GameTree gs (map (go (i+1)) ts)
      | otherwise = GameTree gs []

minimax :: (HivePlayer -> GameState -> Integer) -> HivePlayer -> GameTree -> Integer
minimax s hp (GameTree gs []) = s hp gs
minimax s hp (GameTree gs ts)
  | isPlayersTurn = maximum $ parMap rpar (minimax s hp) ts
  | otherwise = minimum $ parMap rpar (minimax s hp) ts
  where isPlayersTurn = gsCurrPlayer gs == hp

score1 :: HivePlayer -> GameState -> Integer
score1 p gs = case prog of
  Win wp -> if wp == p then 1000 else -1000
  Draw -> -100
  InProgress -> genericLength hand - genericLength startingHand
  where
    prog = gsStatus gs
    hand = currPlayersHand gs
