import Data.Bits (xor)
import Data.Map (Map)
import Data.Map qualified as Map
import Debug.Trace
import Utils.AoC (runner)

type Input = [Int]

type Seq = (Int, Int, Int, Int)

parser :: String -> Input
parser rawInput = map read $ lines rawInput

nextSecret :: Int -> Int
nextSecret x = a
  where
    modulo = 16777216
    y = (x `xor` (64 * x)) `mod` modulo
    z = (y `xor` (y `div` 32)) `mod` modulo
    a = (z `xor` (2048 * z)) `mod` modulo

part1 :: Input -> Int
part1 input = sum $ map (\x -> foldr (\_ v -> nextSecret v) x [1 .. 2000]) input

secretSequence :: Int -> [Int]
secretSequence x = x : secretSequence (nextSecret x)

sequenceToList :: Int -> [Int] -> [(Seq, Int)]
sequenceToList v (a : b : c : d : xs) =
  ((a, b, c, d), v + a + b + c + d) : sequenceToList (v + a) (b : c : d : xs)
sequenceToList _ _ = []

secretToSequenceMap :: Int -> Map Seq Int
secretToSequenceMap x = sequenceMap
  where
    secrets = map (`mod` 10) $ secretSequence x
    priceChanges = zipWith (-) (tail secrets) secrets
    sequenceList = sequenceToList (x `mod` 10) (take 2000 priceChanges)
    sequenceMap = foldl (\map (k, v) -> Map.insertWith seq k v map) Map.empty sequenceList

part2 :: Input -> Int
part2 input = maximum $ Map.elems $ Map.unionsWith (+) $ map secretToSequenceMap input

main :: IO ()
main = interact $ runner parser part1 part2
