module Utils.Grid2D
  ( Dir (..),
    Pos,
    addPos,
    subPos,
    allPos,
    isInGrid,
    isInRange,
    moveDir,
    turnRight,
  )
where

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (a, b) (x, y) = (a + x, b + y)

subPos :: Pos -> Pos -> Pos
subPos (a, b) (x, y) = (a - x, b - y)

data Dir = UP | DOWN | LEFT | RIGHT
  deriving (Show, Ord, Eq)

turnRight :: Dir -> Dir
turnRight UP = RIGHT
turnRight RIGHT = DOWN
turnRight DOWN = LEFT
turnRight LEFT = UP

moveDir :: Dir -> Pos -> Pos
moveDir UP (i, j) = (i - 1, j)
moveDir RIGHT (i, j) = (i, j + 1)
moveDir DOWN (i, j) = (i + 1, j)
moveDir LEFT (i, j) = (i, j - 1)

isInRange :: Int -> Int -> Int -> Bool
isInRange l h v = l <= v && v <= h

isInGrid :: [[a]] -> Pos -> Bool
isInGrid [] _ = False
isInGrid (g : gs) (i, j) = isInRange 0 (length gs) i && isInRange 0 (length g - 1) j

allPos :: [[a]] -> [Pos]
allpos [] = []
allPos (r : rs) = [(i, j) | i <- [0 .. length rs], j <- [0 .. length r - 1]]
