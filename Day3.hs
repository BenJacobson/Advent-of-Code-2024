import Data.Char (digitToInt, isDigit)
import Utils.AoC (runner)

type Input = String

data State = S | M | U | L | PO | D11 | D12 | D13 | C | D21 | D22 | D23 | PC | D | O | N | A | T | PO_DO | PC_DO | PO_DN | PC_DN

getNextState :: State -> Char -> State
getNextState S char
  | char == 'm' = M
  | char == 'd' = D
  | otherwise = S
getNextState M char
  | char == 'u' = U
  | otherwise = S
getNextState U char
  | char == 'l' = L
  | otherwise = S
getNextState L char
  | char == '(' = PO
  | otherwise = S
getNextState PO char
  | isDigit char = D11
  | otherwise = S
getNextState D11 char
  | isDigit char = D12
  | char == ',' = C
  | otherwise = S
getNextState D12 char
  | isDigit char = D13
  | char == ',' = C
  | otherwise = S
getNextState D13 char
  | char == ',' = C
  | otherwise = S
getNextState C char
  | isDigit char = D21
  | otherwise = S
getNextState D21 char
  | isDigit char = D22
  | char == ')' = PC
  | otherwise = S
getNextState D22 char
  | isDigit char = D23
  | char == ')' = PC
  | otherwise = S
getNextState D23 char
  | char == ')' = PC
  | otherwise = S
getNextState D char
  | char == 'o' = O
  | otherwise = S
getNextState O char
  | char == 'n' = N
  | char == '(' = PO_DO
  | otherwise = S
getNextState PO_DO char
  | char == ')' = PC_DO
  | otherwise = S
getNextState N char
  | char == '\'' = A
  | otherwise = S
getNextState A char
  | char == 't' = T
  | otherwise = S
getNextState T char
  | char == '(' = PO_DN
  | otherwise = S
getNextState PO_DN char
  | char == ')' = PC_DN
  | otherwise = S

insertDigit :: Int -> Char -> Int
insertDigit x y = (10 * x) + digitToInt y

process :: String -> State -> Bool -> Bool -> Int -> Int -> Int
process [] _ _ _ _ _ = 0
process (c : s) state b enabled d1 d2 = case getNextState state c of
  S -> process s S b enabled 0 0
  M -> process s M b enabled d1 d2
  U -> process s U b enabled d1 d2
  L -> process s L b enabled d1 d2
  PO -> process s PO b enabled d1 d2
  D11 -> process s D11 b enabled (insertDigit d1 c) d2
  D12 -> process s D12 b enabled (insertDigit d1 c) d2
  D13 -> process s D13 b enabled (insertDigit d1 c) d2
  C -> process s C b enabled d1 d2
  D21 -> process s D21 b enabled d1 (insertDigit d2 c)
  D22 -> process s D22 b enabled d1 (insertDigit d2 c)
  D23 -> process s D23 b enabled d1 (insertDigit d2 c)
  PC -> (if enabled then d1 * d2 else 0) + process s S b enabled 0 0
  D -> process s D b enabled d1 d2
  O -> process s O b enabled d1 d2
  N -> process s N b enabled d1 d2
  A -> process s A b enabled d1 d2
  T -> process s T b enabled d1 d2
  PO_DO -> process s PO_DO b enabled d1 d2
  PC_DO -> process s S b True d1 d2
  PO_DN -> process s PO_DN b enabled d1 d2
  PC_DN -> process s S b b d1 d2

parser :: String -> Input
parser = id

part1 :: Input -> Int
part1 input = process input S True True 0 0

part2 :: Input -> Int
part2 input = process input S False True 0 0

main :: IO ()
main = interact $ runner parser part1 part2
