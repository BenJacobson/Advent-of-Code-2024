import AoC
import Data.List
import Utils (split)

type Input = [(Int, [Int])]

ops :: [Int -> Int -> Int]
ops = [(+), (*)]

strAdd :: Int -> Int -> Int
strAdd x y = read $ show x ++ show y

parseEquation :: String -> (Int, [Int])
parseEquation eq = (read r, map read $ words ns)
  where
    [r, ns] = split (== ':') eq

parser :: String -> Input
parser input = map parseEquation (lines input)

canSolve :: [Int -> Int -> Int] -> Int -> (Int, [Int]) -> Bool
canSolve _ partial (res, []) = res == partial
canSolve ops partial (res, v : vs) = any (\op -> canSolve ops (op partial v) (res, vs)) ops

part1 :: Input -> Int
part1 input = sum $ map fst $ filter (canSolve ops 0) input

part2 :: Input -> Int
part2 input = sum $ map fst $ filter (canSolve (strAdd : ops) 0) input

main :: IO ()
main = interact $ runner parser part1 part2
