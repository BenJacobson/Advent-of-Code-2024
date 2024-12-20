import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Utils.AoC (runner)
import Utils.Grid2D (Pos, allDir, allManhattanFrom, allPos, isInGrid, manhattanDistance, moveDir)

data Cell = FREE | BLOCK
  deriving (Show, Eq)

type Input = ([[Cell]], Pos, Pos)

parseCell :: Char -> Cell
parseCell '#' = BLOCK
parseCell _ = FREE

parser :: String -> Input
parser input = (grid, start, end)
  where
    rawGrid = lines input
    grid = map (map parseCell) rawGrid
    findPos c = head $ filter (\(i, j) -> rawGrid !! i !! j == c) $ allPos rawGrid
    start = findPos 'S'
    end = findPos 'E'

buildDistMap :: [[Cell]] -> Pos -> Int -> Map Pos Int -> Map Pos Int
buildDistMap grid (i, j) dist seen
  | grid !! i !! j /= FREE = Map.empty
  | Map.member (i, j) seen = Map.empty
  | otherwise =
      Map.union nowSeen $
        Map.unionsWith min $
          map (\dir -> buildDistMap grid (moveDir dir (i, j)) (dist + 1) nowSeen) allDir
  where
    nowSeen = Map.insert (i, j) dist seen

producePairPos :: Pos -> Pos -> (Pos, Pos, Int)
producePairPos p1 p2 = (p1, p2, manhattanDistance p1 p2)

liftA2Maybe :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
liftA2Maybe f (Just x) (Just y) = Just (f x y)
liftA2Maybe _ _ _ = Nothing

isFree :: [[Cell]] -> Pos -> Bool
isFree grid (i, j) = isInGrid grid (i, j) && grid !! i !! j == FREE

solve :: Input -> Int -> Int
solve (grid, start, end) n =  length $ filter (>= 100) bestDist
  where
    startDist = buildDistMap grid start 0 Map.empty
    endDist = buildDistMap grid end 0 Map.empty
    totalDist = fromJust $ Map.lookup end startDist
    allCheats = concatMap (\gridPos -> map (producePairPos gridPos) (allManhattanFrom gridPos n)) (allPos grid)
    allowedCheats = filter (\(p1, p2, _) -> isFree grid p1 && isFree grid p2) allCheats
    allDist = mapMaybe (\(p1, p2, add) -> liftA2Maybe (\a b -> a + b + add) (Map.lookup p1 startDist) (Map.lookup p2 endDist)) allowedCheats
    bestDist = map (totalDist -) allDist

part1 :: Input -> Int
part1 input = solve input 2

part2 :: Input -> Int
part2 input = solve input 20

main :: IO ()
main = interact $ runner parser part1 part2
