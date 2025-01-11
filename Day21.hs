import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List (minimum)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Utils.AoC (runner)
import Utils.Grid2D (Dir (..), Pos, moveDir, subPos)
import Utils.List (removeOrderedDuplicates)
import Utils.Memoize (Reduction (..), memoize)

type Input = [String]

parser :: String -> Input
parser = lines

keypad :: Map Char (Int, Int)
keypad =
  Map.fromList
    [ ('7', (0, 0)),
      ('8', (0, 1)),
      ('9', (0, 2)),
      ('4', (1, 0)),
      ('5', (1, 1)),
      ('6', (1, 2)),
      ('1', (2, 0)),
      ('2', (2, 1)),
      ('3', (2, 2)),
      ('0', (3, 1)),
      ('A', (3, 2))
    ]

dirpad =
  Map.fromList
    [ ('^', (0, 1)),
      ('A', (0, 2)),
      ('<', (1, 0)),
      ('v', (1, 1)),
      ('>', (1, 2))
    ]

hPath :: Pos -> Pos -> [Pos]
hPath (i1, j1) (i2, j2) =
  removeOrderedDuplicates $
    concat
      [ [(i1, j) | j <- [j1 .. j2]],
        reverse [(i1, j) | j <- [j2 .. j1]],
        [(i, j2) | i <- [i1 .. i2]],
        reverse [(i, j2) | i <- [i2 .. i1]]
      ]

vPath :: Pos -> Pos -> [Pos]
vPath (i1, j1) (i2, j2) =
  removeOrderedDuplicates $
    concat
      [ [(i, j1) | i <- [i1 .. i2]],
        reverse [(i, j1) | i <- [i2 .. i1]],
        [(i2, j) | j <- [j1 .. j2]],
        reverse [(i2, j) | j <- [j2 .. j1]]
      ]

paths :: Set Pos -> Pos -> Pos -> [[Pos]]
paths s p1 p2 =
  removeOrderedDuplicates $
    filter (all (`Set.member` s)) [hPath p1 p2, vPath p1 p2]

stepToChar :: Pos -> Pos -> Char
stepToChar p1 p2
  | (0, 1) == diff = '>'
  | (0, -1) == diff = '<'
  | (1, 0) == diff = 'v'
  | (-1, 0) == diff = '^'
  where
    diff = p2 `subPos` p1

pathToString :: [Pos] -> String
pathToString (p1 : p2 : ps)
  | p1 == p2 = pathToString (p2 : ps)
  | otherwise = stepToChar p1 p2 : pathToString (p2 : ps)
pathToString _ = "A"

expandMoves :: Map Char Pos -> String -> [[String]]
expandMoves pad s = map (map pathToString) fullPaths
  where
    s' = 'A' : s
    pairs = zip s' (tail s')
    endpoints = map (bimap lookup lookup) pairs
      where
        lookup c = fromJust $ Map.lookup c pad
    fullPaths = map (uncurry (paths (Set.fromList $ Map.elems pad))) endpoints

expand :: (String, Int) -> Reduction (String, Int) Int
expand (code, 0) = Value $ length code
expand (code, n) = reduction
  where
    codeOptionsToSteps :: Int -> [String] -> Reduction (String, Int) Int
    codeOptionsToSteps n codeOptions = Step min (map (,n) codeOptions) (maxBound :: Int)
    expandedCode = expandMoves dirpad code
    reduction = StepList (+) (map (codeOptionsToSteps (n - 1)) expandedCode) 0

allCodes :: [[String]] -> [String]
allCodes [] = [""]
allCodes (sub : rest) = concatMap (\s -> map (s ++) d) sub
  where
    d = allCodes rest

solve :: String -> Int -> Int
solve code n = ans
  where
    expandedSubcodes = allCodes $ expandMoves keypad code
    go subcode = Map.lookup (subcode, n) $ memoize expand Map.empty (subcode, n)
    ans = minimum $ mapMaybe go expandedSubcodes

part1 :: Input -> Int
part1 input = sum $ map (\code -> read (filter isDigit code) * solve code 2) input

part2 :: Input -> Int
part2 input = sum $ map (\code -> read (filter isDigit code) * solve code 25) input

main :: IO ()
main = interact $ runner parser part1 part2
