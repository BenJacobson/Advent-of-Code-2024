import Data.List (intercalate, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Utils.AoC (runner)
import Utils.Memoize (Reduction (..), memoize)
import Utils.String (split, strip)

data Op = AND | OR | XOR
  deriving (Show)

data Wire = Const Bool | Gate Op String String
  deriving (Show)

type Input = Map String Wire

parseOp :: String -> Op
parseOp "AND" = AND
parseOp "OR" = OR
parseOp "XOR" = XOR

parseGate :: String -> (String, Wire)
parseGate rawGate = (result, Gate (parseOp op) op1 op2)
  where
    [op1, op, op2, _, result] = split (== ' ') rawGate

parseConst :: String -> (String, Wire)
parseConst rawConst = (s, Const (b == "1"))
  where
    [s, b] = map strip $ split (== ':') rawConst

parser :: String -> Input
parser rawInput = Map.fromList $ map parseConst consts ++ map parseGate gates
  where
    [consts, gates] = map (lines . strip) $ split (== '$') rawInput

performOp :: Op -> Bool -> Bool -> Bool
performOp AND a b = a && b
performOp OR a b = a || b
performOp XOR a b = a /= b

defaultOp :: Op -> Bool
defaultOp AND = True
defaultOp OR = False
defaultOp XOR = False

wireReduction :: Input -> String -> Reduction String Bool
wireReduction wireMap wireName = case wire of
  Const b -> Value b
  Gate op op1 op2 -> Step (performOp op) [op1, op2] (defaultOp op)
  where
    wire = fromJust $ Map.lookup wireName wireMap

binRead :: [Bool] -> Int
binRead = foldl (\n b -> 2 * n + (if b then 1 else 0)) 0

part1 :: Input -> Int
part1 input = binRead zs
  where
    folder = flip $ memoize $ wireReduction input
    memo = foldr folder Map.empty (Map.keys input)
    zs = map (fromJust . (`Map.lookup` memo)) $ filter ((== 'z') . head) $ map fst $ Map.toDescList input

part2 :: Input -> String
part2 input = intercalate "," $ sort $ concatMap (\(a, b) -> [a, b]) swaps
  where
    -- calculated by hand
    swaps = [("cdj", "z08"), ("gfm", "z32"), ("dhm", "qjd"), ("z16", "mrb")]

main :: IO ()
main = interact $ runner parser part1 part2
