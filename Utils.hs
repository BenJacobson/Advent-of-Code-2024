module Utils
  ( isInRange,
    isInGrid
  )
where

import Data.List

isInRange :: Int -> Int -> Int -> Bool
isInRange l h v = l <= v && v <= h

isInGrid :: [[a]] -> (Int, Int) -> Bool
isInGrid [] _ = False
isInGrid (g : gs) (i, j) = isInRange 0 (length gs) i && isInRange 0 (length g - 1) j
