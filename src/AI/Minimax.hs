{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module AI.Minimax( Value(..)
   , searchMove
   , alphaBeta
   , negascout
   , jamboree
   ) where

import           AI.Gametree
import           Control.Parallel.Strategies
import           Data.Function               (on)
import           Data.List                   (minimumBy)

-- | a type for game position valuations;
-- simply a wrapper newtype over integers
newtype Value = Value Int deriving (Eq, Ord, Enum, Bounded,
                                    Num, Real, Integral, Show, Read)

-- compute best move using some search function
-- undefined for terminal positions
searchMove :: Transitions s l =>
              (Value -> Value -> Int -> s -> Value)
              -> Int -> s -> (l, Value)
searchMove abSearch depth s = cmx (minBound+1) first (transitions s)
  where
    first = head (actions s)
    cmx !alpha best [] = (best, alpha)
    cmx !alpha best ((l,sC):rest) = cmx alpha' best' rest
        where !v = - abSearch (-maxBound) (-alpha) (depth-1) sC
              !alpha' = if v>alpha then v else alpha
              !best' = if v>alpha then l else best

alphaBeta :: Transitions s l =>
             (s -> Value) -> Value -> Value -> Int -> s -> Value
alphaBeta valf alpha beta depth s
  | depth==0 || isTerminal s = valf s
  | otherwise = cmx alpha (successors s)
  where
    cmx !alphaC [] = alphaC
    cmx !alphaC (p:ps)
      | a' >= beta = a'
      | otherwise = cmx (max a' alphaC) ps
        where a' = - alphaBeta valf (-beta) (-alphaC) (depth-1) p

negascout :: Transitions s l =>
             (s -> Value) -> Value -> Value -> Int -> s -> Value
negascout valf alpha beta depth s
  | depth==0 || isTerminal s = valf s
  | depth==1  = - valf s0  -- short-circuit for depth 1
  | b >= beta = b
  | otherwise = scout (max alpha b) b succs
    where
      succs = successors s
      s0 = minimumBy (compare`on`valf) succs
        -- child with best static score
      b = - negascout valf (-beta) (-alpha) (depth-1) s0
        -- full search estimate for the best child

      scout _ !bC [] = bC
      scout !alphaC !bC (p:ps)
        | sC >= beta = sC
        | otherwise = scout alpha' b' ps
          where sC = - negascout valf (-(1+alphaC)) (-alphaC) (depth-1) p
                s' | sC > alphaC = - negascout valf (-beta) (-alphaC) (depth-1) p
                   | otherwise = sC
                alpha' = max alphaC s'
                b' = max bC s'

-- | Parallel negascout, aka "Jamboree"
-- | result of each scout test
data Result a b = Cutoff a   -- beta cutoff found
                | Search b   -- do a full search
                | OK         -- test suceeded



jamboree :: Transitions s l =>
            (s -> Value) -> Value -> Value -> Int -> s -> Value
jamboree valf alpha beta depth p
  | depth<=1 = negascout valf alpha beta depth p
            -- use sequencial version for low depth
  | isTerminal p = valf p   -- terminal node?
  | b >= beta = b           -- 1st child failed high
  | otherwise = cutoff [] (map scout ps `using` parList rseq)
    where
      ps = successors p
      p0 = minimumBy (compare`on`valf) ps      -- estimated best child
      b =  - jamboree valf (-beta) (-alpha) (depth-1) p0  -- full search estimate
      alpha' = max alpha b

      scout pS
        | s >= beta = Cutoff s
        | s > alpha' = Search pS
        | otherwise = OK
          where s = - jamboree valf (-(1+alpha')) (-alpha') (depth-1) pS
                    -- null window search

      -- join results of parallel scouts
      cutoff _  (Cutoff s : _) = s
      cutoff psC (Search pC : rs) = cutoff (pC:psC) rs
      cutoff psC (OK : rs)       = cutoff psC rs
      cutoff psC []              = search alpha' b psC

      -- sequential full search for scout failures
      search _ !bC [] = bC
      search !alphaC !bC (pC : pCs)
        | s >= beta = s
        | otherwise = search (max s alphaC) (max s bC) pCs
          where s = - jamboree valf (-beta) (-alphaC)  (depth-1) pC
