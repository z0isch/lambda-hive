{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module AI.Gametree where

-- | a type class for labelled transition systems
-- i.e. game positions and moves
class Transitions s l | s -> l where
  actions :: s -> [l]         -- next labels from a state
  transition :: l -> s -> s   -- transition from a state

-- | check if a state is terminal
-- i.e. has no actions
isTerminal :: Transitions s l => s -> Bool
isTerminal = null . actions

-- | immediate state successors
successors :: Transitions s l => s -> [s]
successors s = [transition l s | l <- actions s]

-- | immediate successors labelled by actions
transitions :: Transitions s l => s -> [(l,s)]
transitions s = [(l, transition l s) | l <- actions s]
