import Data.List (repeat, reverse, take)
import Data.Maybe (fromMaybe)
import Utils.AoC (runner)
import Utils.List (setSubarray, subarrayIndex)
import Utils.String (strip)

type Input = [Int]

generateFiles :: [Int] -> Int -> Bool -> [Int]
generateFiles [] _ _ = []
generateFiles (f : fs) fid isFile =
  replicate f (if isFile then fid else -1)
    ++ generateFiles fs (if isFile then fid else fid + 1) (not isFile)

parser :: String -> Input
parser input = generateFiles sizes 0 True
    where
        sizes = map (read . (: [])) (strip input)

makeContiguous :: [Int] -> [Int] -> Int -> [Int]
makeContiguous [] _ _ = []
makeContiguous _ [] _ = []
makeContiguous _ _ 0 = []
makeContiguous (a : as) (b : bs) n
  | a /= -1 = a : makeContiguous as (b : bs) (n - 1)
  | b == -1 = makeContiguous (a : as) bs n
  | otherwise = b : makeContiguous as bs (n - 1)

checksum :: [Int] -> Int -> Int
checksum [] _ = 0
checksum ((-1) : fs) i = checksum fs (i + 1)
checksum (f : fs) i = (f * i) + checksum fs (i + 1)

part1 :: Input -> Int
part1 input = checksum (makeContiguous input (reverse input) (length $ filter (/= (-1)) input)) 0

defrag :: [Int] -> Int -> [Int]
defrag fs (-1) = fs
defrag fs x =
  if newIndex < currentIndex
    then defrag (setSubarray (setSubarray fs (replicate count x) newIndex) (replicate count (-1)) currentIndex) (x - 1)
    else defrag fs (x - 1)
  where
    count = length $ filter (== x) fs
    currentIndex = fromMaybe (-1) $ subarrayIndex fs (replicate count x) 0
    newIndex = fromMaybe (length fs) $ subarrayIndex fs (replicate count (-1)) 0

part2 :: Input -> Int
part2 input = checksum (defrag input (foldr max 0 input)) 0

main :: IO ()
main = interact $ runner parser part1 part2
