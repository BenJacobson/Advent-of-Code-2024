module Utils.Memoize (Reduction (..), memoize) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)

data Reduction a b
  = Value b
  | Step (b -> b -> b) [a] b
  | StepList (b -> b -> b) [Reduction a b] b

memoize :: (Ord a) => (a -> Reduction a b) -> Map a b -> a -> Map a b
memoize reducer memo arg
  | Map.member arg memo = memo
  | otherwise = Map.insert arg ans memo'
  where
    (ans, memo') = reduce (reducer arg) (flip $ memoize reducer) memo

reduce :: (Ord a) => Reduction a b -> (a -> Map a b -> Map a b) -> Map a b -> (b, Map a b)
reduce (Value ans) _ memo = (ans, memo)
reduce (Step combine parts def) f memo = (ans, memo')
  where
    memo' = foldr f memo parts
    vals = map (fromJust . (`Map.lookup` memo')) parts
    ans = foldr combine def vals
reduce (StepList combine parts def) f memo = foldr go (def, memo) parts
  where
    go steps (acc, memo') = (combine ans acc, memo'')
      where
        (ans, memo'') = reduce steps f memo'
