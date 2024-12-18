import Data.Set (Set, empty, singleton, unions)
import Utils.AoC (runner)
import Utils.Grid2D (Dir (..), Pos, allPos, isInGrid, moveDir)

type Input = [[Int]]

parser :: String -> Input
parser input = map (map (read . (: []))) (lines input)

countTrails :: Input -> Pos -> Int -> Set Pos
countTrails grid (i, j) n
  | not $ isInGrid grid (i, j) = empty
  | grid !! i !! j /= n = empty
  | n == 9 = singleton (i, j)
  | otherwise =
      unions $
        map (\dir -> countTrails grid (moveDir dir (i, j)) (n + 1)) [UP, DOWN, LEFT, RIGHT]

part1 :: Input -> Int
part1 grid = sum $ map (\pos -> length $ countTrails grid pos 0) (allPos grid)

countPaths :: Input -> Pos -> Int -> Int
countPaths grid (i, j) n
  | not $ isInGrid grid (i, j) = 0
  | grid !! i !! j /= n = 0
  | n == 9 = 1
  | otherwise =
      sum $
        map (\dir -> countPaths grid (moveDir dir (i, j)) (n + 1)) [UP, DOWN, LEFT, RIGHT]

part2 :: Input -> Int
part2 grid = sum $ map (\pos -> countPaths grid pos 0) (allPos grid)

main :: IO ()
main = interact $ runner parser part1 part2
