module Utils.Grid2D
  ( Dir (..),
    allPos,
    isInGrid,
    isInRange,
    moveDir,
    turnRight
  )
where

data Dir = UP | DOWN | LEFT | RIGHT
  deriving (Show, Ord, Eq)

turnRight :: Dir -> Dir
turnRight UP = RIGHT
turnRight RIGHT = DOWN
turnRight DOWN = LEFT
turnRight LEFT = UP

moveDir :: Dir -> (Int, Int) -> (Int, Int)
moveDir UP (i, j) = (i - 1, j)
moveDir RIGHT (i, j) = (i, j + 1)
moveDir DOWN (i, j) = (i + 1, j)
moveDir LEFT (i, j) = (i, j - 1)

isInRange :: Int -> Int -> Int -> Bool
isInRange l h v = l <= v && v <= h

isInGrid :: [[a]] -> (Int, Int) -> Bool
isInGrid [] _ = False
isInGrid (g : gs) (i, j) = isInRange 0 (length gs) i && isInRange 0 (length g - 1) j

allPos :: [[a]] -> [(Int, Int)]
allpos [] = []
allPos (r : rs) = [(i, j) | i <- [0 .. length rs], j <- [0 .. length r - 1]]

