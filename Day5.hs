import qualified Prelude
import Prelude hiding (lookup)
import AoC (runner)
import Data.Char (isSpace)
import Data.List (elemIndex, sortBy)
import Data.Text (Text, pack, split, strip, unpack)
import Utils (fromTupleList)
import Data.Map (findWithDefault)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShow, traceShowId)

type Input = ([(Int, Int)], [[Int]])

parseRule :: Text -> (Int, Int)
parseRule rule = (first, last)
  where
    [first, last] = map (read . unpack) (split (== '|') rule)

parsePages :: Text -> [Int]
parsePages pages = map (read . unpack) (split (== ',') pages)

parser :: String -> Input
parser input = (map parseRule rules, map parsePages pages)
  where
    [rules, pages] = map (split isSpace . strip) $ split (== '$') (pack input)

isInOrder :: [Int] -> (Int, Int) -> Bool
isInOrder pages (first, last) =
  case (elemIndex first pages, elemIndex last pages) of
    (Just firstIndex, Just lastIndex) -> firstIndex < lastIndex
    _ -> True

isAllInOrder :: [(Int, Int)] -> [Int] -> Bool
isAllInOrder rules pages = all (isInOrder pages) rules

mid :: [Int] -> Int
mid l = l !! (length l `div` 2)

part1 :: Input -> Int
part1 (rules, pages) = sum $ map mid $ filter (isAllInOrder rules) pages

part2 :: Input -> Int
part2 (rules, pages) =
    sum
    $ map (mid . sortPages) oooPages
    where
        oooPages = filter (not . isAllInOrder rules) pages
        orderMap = fromTupleList rules
        sortPages = until (isAllInOrder rules) applyOrder
        applyOrder [] = []
        applyOrder [x] = [x]
        applyOrder (x : y : ps) = if x `elem` findWithDefault [] y orderMap then applyOrder (y : x : ps) else x : applyOrder (y : ps)

main :: IO ()
main = interact $ runner parser part1 part2
