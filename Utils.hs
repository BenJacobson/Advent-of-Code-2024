module Utils
  ( isInRange,
    isInGrid,
    freqMap,
    fromTupleList
  )
where

import Data.List
import Data.Map (Map, empty, insertWith)

isInRange :: Int -> Int -> Int -> Bool
isInRange l h v = l <= v && v <= h

isInGrid :: [[a]] -> (Int, Int) -> Bool
isInGrid [] _ = False
isInGrid (g : gs) (i, j) = isInRange 0 (length gs) i && isInRange 0 (length g - 1) j

freqMap :: Ord a => [a] -> Map a Int
freqMap = foldr (\x m -> insertWith (+) x 1 m) empty

fromTupleList :: Ord a => [(a, b)] -> Map a [b]
fromTupleList = foldr (\(a, b) m -> insertWith (++) a [b] m) empty
