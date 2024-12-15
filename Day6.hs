import Data.Set (Set, empty, insert, member)
import Utils.AoC (runner)
import Utils.Grid2D (Dir (..), allPos, isInGrid, moveDir, turnRight)
import Utils.List (modifyAt, setAt)

data Loc = Loc
  { pos :: (Int, Int),
    dir :: Dir
  }
  deriving (Show, Ord, Eq)

data Cell = FREE | BLOCK
  deriving (Show, Eq)

type Input = ([[Cell]], Loc)

parseLoc :: [String] -> (Int, Int) -> Loc
parseLoc [] _ = error "Failed to parse location"
parseLoc ("" : ss) (i, j) = parseLoc ss (i + 1, 0)
parseLoc ((c : s) : ss) (i, j) = case c of
  '^' -> Loc {pos = (i, j), dir = UP}
  _ -> parseLoc (s : ss) (i, j + 1)

mapCell :: Char -> Cell
mapCell '#' = BLOCK
mapCell _ = FREE

parser :: String -> Input
parser s = (map (map mapCell) grid, parseLoc grid (0, 0))
  where
    grid = lines s

getCell :: [[Cell]] -> (Int, Int) -> Cell
getCell grid pos
  | isInGrid grid pos = grid !! fst pos !! snd pos
  | otherwise = FREE

setCell :: [[Cell]] -> (Int, Int) -> Cell -> [[Cell]]
setCell grid (i, j) v
  | isInGrid grid (i, j) = modifyAt grid i (\r -> setAt r j v)
  | otherwise = grid

pathLength :: Input -> Set (Int, Int) -> Int
pathLength (grid, Loc {pos = pos, dir = dir}) seen
  | not $ isInGrid grid pos = length seen
  | otherwise = case getCell grid newPos of
      BLOCK -> pathLength (grid, Loc {pos = pos, dir = turnRight dir}) seen
      FREE -> pathLength (grid, Loc {pos = newPos, dir = dir}) (insert newPos seen)
  where
    newPos = moveDir dir pos

part1 :: Input -> Int
part1 input = pathLength input (insert (pos $ snd input) empty) - 1

hasPathCycle :: Input -> Set Loc -> Bool
hasPathCycle (grid, Loc {pos = pos, dir = dir}) seen
  | not $ isInGrid grid pos = False
  | otherwise = case getCell grid newPos of
      BLOCK -> hasPathCycle (grid, Loc {pos = pos, dir = turnRight dir}) seen
      FREE -> member newLoc seen || hasPathCycle (grid, newLoc) (insert newLoc seen)
  where
    newPos = moveDir dir pos
    newLoc = Loc {pos = newPos, dir = dir}

part2 :: Input -> Int
part2 (grid, loc) =
  length $
    filter id $
      map (\blockPos -> hasPathCycle (setCell grid blockPos BLOCK, loc) (insert loc empty)) $
        filter (\p -> getCell grid p == FREE && p /= pos loc) (allPos grid)

main :: IO ()
main = interact $ runner parser part1 part2
