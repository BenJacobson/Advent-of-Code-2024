import AoC
import Data.List

type Input = [[Int]]

parser :: String -> Input
parser input = map (map read . words) (lines input)

part1 :: Input -> Int
part1 input = sum $ map (\[x, y] -> abs (x - y)) (transpose (map sort (transpose input)))

count :: [[Int]] -> Int
count [xs, ys] = sum $ filter (/= 0) (map (\x -> length (filter (== x) ys) * x) xs)

part2 :: Input -> Int
part2 input = count (transpose input)

main :: IO ()
main = interact $ runner parser part1 part2
