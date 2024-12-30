import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace (traceShow, traceShowId)
import Utils.AoC (runner)
import Utils.Map (freqMap)

type Input = [Int]

parser :: String -> Input
parser input = map read (words input)

blink :: Int -> [Int]
blink 0 = [1]
blink n =
  if even $ length strn
    then [left, right]
    else [2024 * n]
  where
    strn = show n
    half = length strn `div` 2
    left = read $ take half strn
    right = read $ drop half strn

insertAll :: [Int] -> Int -> Map Int Int -> Map Int Int
insertAll [] _ m = m
insertAll (k : ks) v m = insertAll ks v $ Map.insertWith (+) k v m

blinkMap :: Map Int Int -> Map Int Int
blinkMap = Map.foldrWithKey (insertAll . blink) Map.empty

applyBlinks :: Int -> Map Int Int -> Map Int Int
applyBlinks 0 m = m
applyBlinks n m = applyBlinks (n - 1) (blinkMap m)

part1 :: Input -> Int
part1 input = sum $ Map.elems $ applyBlinks 25 $ freqMap input

part2 :: Input -> Int
part2 input = sum $ Map.elems $ applyBlinks 75 $ freqMap input

main :: IO ()
main = interact $ runner parser part1 part2
