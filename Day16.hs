import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Utils.AoC (runner)
import Utils.BFS (bfs)
import Utils.Grid2D (Dir (..), Pos, allDir, allPos, moveDir, turnLeft, turnRight)

data Cell = BLOCK | FREE
  deriving (Eq)

type Grid = [[Cell]]

type Vert = (Pos, Dir)

type Input = (Pos, Pos, Grid)

parseCell :: Char -> Cell
parseCell '#' = BLOCK
parseCell _ = FREE

parser :: String -> Input
parser rawInput = (startPos, endPos, grid)
  where
    rawGrid = lines rawInput
    startPos = head $ filter (\(i, j) -> rawGrid !! i !! j == 'S') $ allPos rawGrid
    endPos = head $ filter (\(i, j) -> rawGrid !! i !! j == 'E') $ allPos rawGrid
    grid = map (map parseCell) rawGrid

neighbors :: Grid -> (Vert, Int) -> [(Vert, Int)]
neighbors grid ((pos, dir), cost) =
  if isStraightBlocked
    then [left, right]
    else [straight, left, right]
  where
    straightPos@(si, sj) = moveDir dir pos
    isStraightBlocked = grid !! si !! sj == BLOCK
    straight = ((straightPos, dir), cost + 1)
    left = ((pos, turnLeft dir), cost + 1000)
    right = ((pos, turnRight dir), cost + 1000)

part1 :: Input -> Int
part1 (start, end, grid) = cost
  where
    startSet = Set.singleton (0, (start, RIGHT), (start, RIGHT))
    costMap = bfs (neighbors grid) ((end ==) . fst) startSet Map.empty
    cost = minimum $ map fst $ mapMaybe ((`Map.lookup` costMap) . (end,)) allDir

optimalPath :: Map Vert (Int, [Vert]) -> Vert -> Set Vert -> Set Vert
optimalPath paths current seen
  | Set.member current seen = seen
  | otherwise = foldr (optimalPath paths) (Set.insert current seen) neighbors
  where
    neighbors = maybe [] snd (Map.lookup current paths)

part2 :: Input -> Int
part2 (start, end, grid) = length path
  where
    startSet = Set.singleton (0, (start, RIGHT), (start, RIGHT))
    costMap = bfs (neighbors grid) (const False) startSet Map.empty
    cost = minimum $ map fst $ mapMaybe ((`Map.lookup` costMap) . (end,)) allDir
    endVerts =
      mapMaybe
        ( \d ->
            Map.lookup (end, d) costMap
              >>= (\(endCost, _) -> if endCost == cost then Just (end, d) else Nothing)
        )
        allDir
    path = Set.map fst $ foldr (optimalPath costMap) Set.empty endVerts

main :: IO ()
main = interact $ runner parser part1 part2
