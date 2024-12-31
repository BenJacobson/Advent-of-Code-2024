import Utils.AoC (runner)
import Utils.Grid2D (Pos)
import Utils.String (split, strip)

type Input = [(Pos, Pos, Pos)]

parseEquation :: String -> Pos
parseEquation rawInput = (read xStr, read yStr)
  where
    [_, values] = split (== ':') $ strip rawInput
    [xEq, yEq] = split (== ',') $ strip values
    xStr = drop 2 $ strip xEq
    yStr = drop 2 $ strip yEq

parseEquationSet :: String -> (Pos, Pos, Pos)
parseEquationSet rawInput = (parseEquation a, parseEquation b, parseEquation prize)
  where
    [a, b, prize] = split (== '\n') $ strip rawInput

parser :: String -> Input
parser rawInput = map parseEquationSet $ split (== '$') $ strip rawInput

countTokens :: (Pos, Pos, Pos) -> Int
countTokens ((x1, y1), (x2, y2), (xt, yt))
  | r1 /= 0 = 0
  | r2 /= 0 = 0
  | a < 0 = 0
  | b < 0 = 0
  | otherwise = 3*a + b
  where
    numer1 = yt*x1 - y1*xt
    denom1 = y2*x1 - y1*x2
    r1 = numer1 `rem` denom1
    b = numer1 `div` denom1
    numer2 = xt - b*x2
    denom2 = x1
    r2 = numer2 `rem` denom2
    a = numer2 `div` denom2

part1 :: Input -> Int
part1 input = sum $ map countTokens input

modifyEquationForPartTwo :: (Pos, Pos, Pos) -> (Pos, Pos, Pos)
modifyEquationForPartTwo (one, two, (xt, yt)) = (one, two, (xt + 10000000000000, yt + 10000000000000))

part2 :: Input -> Int
part2 input = sum $ map (countTokens . modifyEquationForPartTwo) input

main :: IO ()
main = interact $ runner parser part1 part2
