module Utils.Grid2D
  ( Dir (..),
    Pos,
    addPos,
    subPos,
    allPos,
    isInGrid,
    isInRange,
    allDir,
    moveDir,
    turnRight,
    turnLeft,
    manhattanDistance,
    allManhattan,
    allManhattanFrom,
    getCell,
    setCell,
    traceGrid,
  )
where

import Debug.Trace (traceShowId)
import Utils.List (modifyAt, setAt)

type Pos = (Int, Int)

addPos :: Pos -> Pos -> Pos
addPos (a, b) (x, y) = (a + x, b + y)

subPos :: Pos -> Pos -> Pos
subPos (a, b) (x, y) = (a - x, b - y)

data Dir = UP | DOWN | LEFT | RIGHT
  deriving (Show, Ord, Eq, Enum, Bounded)

allDir :: [Dir]
allDir = [minBound .. maxBound]

turnLeft :: Dir -> Dir
turnLeft UP = LEFT
turnLeft RIGHT = UP
turnLeft DOWN = RIGHT
turnLeft LEFT = DOWN

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

getCell :: [[a]] -> Pos -> Maybe a
getCell grid pos@(i, j)
  | isInGrid grid pos = Just $ grid !! i !! j
  | otherwise = Nothing

setCell :: [[a]] -> Pos -> a -> [[a]]
setCell grid (i, j) v
  | isInGrid grid (i, j) = modifyAt grid i (\r -> setAt r j v)
  | otherwise = grid

allPos :: [[a]] -> [Pos]
allpos [] = []
allPos (r : rs) = [(i, j) | i <- [0 .. length rs], j <- [0 .. length r - 1]]

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (i1, j1) (i2, j2) = abs (i1 - i2) + abs (j1 - j2)

allManhattan :: Int -> [Pos]
allManhattan n = [(i, j) | i <- [-n .. n], j <- [-n .. n], manhattanDistance (0, 0) (i, j) <= n]

allManhattanFrom :: Pos -> Int -> [Pos]
allManhattanFrom (di, dj) n = [(i + di, j + dj) | (i, j) <- allManhattan n]

traceGrid :: (Show a) => [[a]] -> [[a]]
traceGrid = map traceShowId
