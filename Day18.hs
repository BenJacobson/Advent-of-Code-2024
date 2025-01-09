import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Utils.AoC (runner)
import Utils.BFS (bfs)
import Utils.Grid2D (Pos, allDir, isInRange, moveDir)
import Utils.String (split)

type Input = (Int, Int, [(Int, Int)])

parser :: String -> Input
parser rawInput = (part1Coords, size, coords)
  where
    rawFirstLine : rawCoords = lines rawInput
    [part1Coords, size] = map read $ split (== ',') rawFirstLine
    coords :: [(Int, Int)]
    coords = map ((\[i, j] -> (i, j)) . map read . split (== ',')) rawCoords

adj :: Int -> Set Pos -> (Pos, Int) -> [(Pos, Int)]
adj size blocked (pos, cost) =
  filter isFree $ map (\d -> (moveDir d pos, cost + 1)) allDir
  where
    isFree (pos@(i, j), _) =
      isInRange 0 size i
        && isInRange 0 size j
        && not (Set.member pos blocked)

part1 :: Input -> Int
part1 (part1Coords, size, coords) = cost
  where
    blockedSet = Set.fromList $ take part1Coords coords
    startSet = Set.singleton (0, (0, 0), (0, 0))
    costMap = bfs (adj size blockedSet) (const False) startSet Map.empty
    cost = maybe (-1) fst (Map.lookup (size, size) costMap)

findBlockingCoord :: Input -> (Int, Int) -> Maybe Pos
findBlockingCoord input@(_, size, coords) (low, high)
  | n > length coords = Nothing
  | (low + 1) == high = Just (coords !! low)
  | otherwise =
      if Map.member (size, size) costMap
        then findBlockingCoord input (n, high)
        else findBlockingCoord input (low, n)
  where
    n = (low + high) `div` 2
    blockedSet = Set.fromList $ take n coords
    startSet = Set.singleton (0, (0, 0), (0, 0))
    costMap = bfs (adj size blockedSet) (const False) startSet Map.empty

part2 :: Input -> Maybe Pos
part2 input@(_, _, coords) = findBlockingCoord input (1, length coords + 2)

main :: IO ()
main = interact $ runner parser part1 part2
