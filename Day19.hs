import Data.List (isPrefixOf)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Utils.AoC (runner)
import Utils.String (split, strip)

type Input = ([String], [String])

parser :: String -> Input
parser input = (patterns, designs)
  where
    [first, second] = map strip (split (== '$') input)
    patterns = map strip (split (== ',') first)
    designs = lines second

isBuildable :: [String] -> String -> Map String Int
isBuildable _ "" = Map.singleton "" 1
isBuildable patterns design =
  Map.insert design possibleWays following
  where
    following = isBuildable patterns $ tail design
    applicablePatterns = filter (`isPrefixOf` design) patterns
    getValue p = fromMaybe 0 $ Map.lookup (drop (length p) design) following
    possibleWays = sum $ map getValue applicablePatterns

part1 :: Input -> Int
part1 (patterns, designs) =
  length $
    filter (> 0) $
      map (\design -> fromMaybe 0 $ Map.lookup design (isBuildable patterns design)) designs

part2 :: Input -> Int
part2 (patterns, designs) =
  sum $
    map (\design -> fromMaybe 0 $ Map.lookup design (isBuildable patterns design)) designs

main :: IO ()
main = interact $ runner parser part1 part2
