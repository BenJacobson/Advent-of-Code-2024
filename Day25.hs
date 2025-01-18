import Data.List (partition)
import Utils.AoC (runner)
import Utils.List (count)
import Utils.String (split, strip)

type KeyLock = [Int]

type Input = ([KeyLock], [KeyLock])

parseKeyLock :: [String] -> KeyLock
parseKeyLock ([] : xs) = []
parseKeyLock xs = count ((== '#') . head) xs : parseKeyLock (map tail xs)

parser :: String -> Input
parser rawInput = (keys, locks)
  where
    keysAndLocks = map strip $ split (== '$') rawInput
    (rawKeys, rawLocks) = partition ((== '.') . head) keysAndLocks
    keys = map (parseKeyLock . lines) rawKeys
    locks = map (parseKeyLock . lines) rawLocks

part1 :: Input -> Int
part1 (keys, locks) =
  length [0 | key <- keys, lock <- locks, all (< 8) $ zipWith (+) key lock]

part2 :: Input -> Int
part2 input = 0

main :: IO ()
main = interact $ runner parser part1 part2
