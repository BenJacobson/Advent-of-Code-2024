import Utils.AoC (runner)

type Input = [[Int]]

parser :: String -> Input
parser input = map (map read . words) (lines input)

isInRange :: Int -> Int -> Int -> Bool
isInRange l h v = l <= v && v <= h

isSafe :: (Int -> Int -> Bool) -> [Int] -> Bool
isSafe _ [] = True
isSafe _ [_] = True
isSafe p (x : y : xs) = p x y && isSafe p (y : xs)

isSafeAbs :: [Int] -> Bool
isSafeAbs xs = isSafe (\x y -> isInRange 1 3 (y - x)) xs || isSafe (\x y -> isInRange (-3) (-1) (y - x)) xs

part1 :: Input -> Int
part1 input = length $ filter isSafeAbs input

dropN :: Int -> [Int] -> [Int]
dropN _ [] = []
dropN 0 (x : xs) = xs
dropN n (x : xs) = x : dropN (n - 1) xs

part2 :: Input -> Int
part2 input = length $ filter (\xs -> any isSafeAbs [dropN i xs | i <- [0 .. (length xs - 1)]]) input

main :: IO ()
main = interact $ runner parser part1 part2
