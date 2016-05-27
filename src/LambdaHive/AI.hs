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
  | otherwise = do
    let possibleStates = doAI ai gs
    r <- randomRIO (0,length possibleStates - 1)
    return $ possibleStates !! r
  where
    allMoves = validPlayerMoves gs

doAI RandomAI gs = map (fromJust . makeMove gs) $ validPlayerMoves gs
doAI (Minimax i s) gs = bestStates
  where
    (Node _ ts) = prune i $ buildTree gs
    stateScores = map (\t -> (minimax False s t,t)) ts
    bestScore = fst $ last $ sortOn fst stateScores
    bestStates = map ((\(Node m _) -> m) . snd) $ filter ((==) bestScore . fst) stateScores

buildTree :: GameState -> GameTree GameState
buildTree gs = Node gs $ map buildTree cGs
  where cGs = map (fromJust . makeMove gs) $ validPlayerMoves gs

prune :: Int -> GameTree a -> GameTree a
prune 0 (Node t _) = Node t []
prune n (Node t ts) = Node t $ map (prune (n-1)) ts

minimax :: Bool -> ScoreAlgorithm -> GameTree GameState -> Integer
minimax _ s (Node hms []) = s hms
minimax True s (Node _ ts) = maximum $ parMap rseq (minimax False s) ts
minimax False s (Node _ ts) = minimum $ parMap rseq (minimax True s) ts

score1 :: ScoreAlgorithm
score1 gs = case prog of
  Win wp -> if wp == p then 1000 else -1000
  Draw -> -100
  InProgress -> sum artPiecePoints
              + (5 * fromMaybe 0 (breathingRoom <$> queen oposingPlayer))
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
