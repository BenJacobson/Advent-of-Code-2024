import Utils.AoC (runner)
import Utils.Grid2D (isInGrid)

type Input = [[Char]]

dirs =
  [ (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1),
    (1, 1),
    (1, -1),
    (-1, 1),
    (-1, -1)
  ]

testWord :: Input -> (Int, Int) -> String -> (Int, Int) -> Bool
testWord _ _ "" _ = True
testWord grid (i, j) (c : word) (di, dj) =
  isInGrid grid (i, j)
    && grid !! i !! j == c
    && testWord grid (i + di, j + dj) word (di, dj)

searchWord :: Input -> String -> Int
searchWord grid word =
  length
    . filter id
    $ [ testWord grid (i, j) word dir
        | i <- [0 .. length grid - 1],
          j <- [0 .. length (head grid) - 1],
          dir <- dirs
      ]

testSides :: (Char, Char) -> (Char, Char) -> Bool
testSides (a, b) (c, d) = (a == c && b == d) || (a == d && b == c)

getLeftSides :: Input -> (Int, Int) -> (Char, Char)
getLeftSides grid (i, j) = (grid !! (i - 1) !! (j - 1), grid !! (i + 1) !! (j + 1))

getRightSides :: Input -> (Int, Int) -> (Char, Char)
getRightSides grid (i, j) = (grid !! (i - 1) !! (j + 1), grid !! (i + 1) !! (j - 1))

testX :: Input -> Char -> (Char, Char) -> (Int, Int) -> Bool
testX grid middle sides (i, j) =
  grid !! i !! j == middle
    && testSides sides (getLeftSides grid (i, j))
    && testSides sides (getRightSides grid (i, j))

searchX :: Input -> Char -> (Char, Char) -> Int
searchX grid middle sides =
  length
    . filter id
    $ [ testX grid 'A' ('M', 'S') (i, j)
        | i <- [1 .. length grid - 2],
          j <- [1 .. length (head grid) - 2]
      ]

parser :: String -> Input
parser = lines

part1 :: Input -> Int
part1 input = searchWord input "XMAS"

part2 :: Input -> Int
part2 input = searchX input 'A' ('M', 'S')

main :: IO ()
main = interact $ runner parser part1 part2
