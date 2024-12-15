module Utils.Map
  ( freqMap,
    fromTupleList,
  )
where

import Data.Map (Map, empty, insertWith)

freqMap :: (Ord a) => [a] -> Map a Int
freqMap = foldr (\x m -> insertWith (+) x 1 m) empty

fromTupleList :: (Ord a) => [(a, b)] -> Map a [b]
fromTupleList = foldr (\(a, b) m -> insertWith (++) a [b] m) empty
