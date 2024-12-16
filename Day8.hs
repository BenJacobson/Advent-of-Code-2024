import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Utils.AoC (runner)
import Utils.Grid2D (Pos, addPos, allPos, isInGrid, isInRange, subPos)
import Utils.Map (fromTupleList)

type Input = ([[Char]], Map Char [Pos])

parseAntennaMap :: [[Char]] -> Map Char [Pos]
parseAntennaMap grid =
  fromTupleList $
    map (\(i, j) -> (grid !! i !! j, (i, j))) $
      filter (\(i, j) -> grid !! i !! j /= '.') $
        allPos grid

parser :: String -> Input
parser input = (grid, parseAntennaMap grid)
  where
    grid = lines input

antinodes :: Pos -> Pos -> [Pos]
antinodes a b = [a `addPos` a `subPos` b, b `addPos` b `subPos` a]

allAntinodes :: (Pos -> Pos -> [Pos]) -> [Pos] -> [Pos]
allAntinodes f nodes =
  concat
    [ f (nodes !! i) (nodes !! j)
      | i <- [0 .. length nodes - 1],
        j <- [i + 1 .. length nodes - 1]
    ]

part1 :: Input -> Int
part1 (grid, antennas) =
  length $
    Set.fromList $
      concatMap (filter (isInGrid grid) . allAntinodes antinodes) (Map.elems antennas)

boundedAntinodes :: [[Char]] -> Pos -> Pos -> [Pos]
boundedAntinodes grid a b = go grid a (a `subPos` b) ++ go grid a (b `subPos` a)
  where
    go grid p dp = if isInGrid grid p then p : go grid (p `addPos` dp) dp else []

part2 :: Input -> Int
part2 (grid, antennas) =
  length $
    Set.fromList $
      concatMap (filter (isInGrid grid) . allAntinodes (boundedAntinodes grid)) (Map.elems antennas)

main :: IO ()
main = interact $ runner parser part1 part2
