import Data.Bits (xor, (.|.))
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Utils.AoC (runner)
import Utils.String (split)

data Program = Program
  { pc :: Int,
    regA :: Int,
    regB :: Int,
    regC :: Int,
    code :: [Int],
    output :: [Int]
  }
  deriving (Show)

data Opcode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
  deriving (Eq, Show)

readOpcode :: Int -> Opcode
readOpcode 0 = Adv
readOpcode 1 = Bxl
readOpcode 2 = Bst
readOpcode 3 = Jnz
readOpcode 4 = Bxc
readOpcode 5 = Out
readOpcode 6 = Bdv
readOpcode 7 = Cdv

parser :: String -> Program
parser rawInput =
  Program
    { pc = 0,
      regA = regA,
      regB = regB,
      regC = regC,
      code = code,
      output = []
    }
  where
    inputLines = lines rawInput
    [regA, regB, regC] = map (read . last . split (== ' ')) (take 3 inputLines)
    code = map read $ split (== ',') $ last $ split (== ' ') $ last inputLines

runProgram :: Program -> Program
runProgram
  program@Program
    { pc = pc,
      regA = regA,
      regB = regB,
      regC = regC,
      code = code,
      output = output
    }
    | pc >= length code = program
    | otherwise = runProgram $ case opcode of
        Adv -> advPc {regA = regA `div` (2 ^ combo)}
        Bxl -> advPc {regB = regB `xor` literal}
        Bst -> advPc {regB = combo `mod` 8}
        Jnz -> if regA == 0 then advPc else program {pc = literal}
        Bxc -> advPc {regB = regB `xor` regC}
        Out -> advPc {output = (combo `mod` 8) : output}
        Bdv -> advPc {regB = regA `div` (2 ^ combo)}
        Cdv -> advPc {regC = regA `div` (2 ^ combo)}
    where
      opcode = readOpcode $ code !! pc
      literal = code !! (pc + 1)
      combo = case literal of
        4 -> regA
        5 -> regB
        6 -> regC
        v -> v
      advPc = program {pc = pc + 2}

part1 :: Program -> String
part1 program = intercalate "," $ map show $ reverse $ output finishedProgram
  where
    finishedProgram = runProgram program

findRegA :: Program -> [Int] -> Int -> Maybe Int
findRegA _ [] regA = Just regA
findRegA program (t : ts) regA = newRegA
  where
    upRegA = regA * 8
    newRegAOptions =
      filter (\tryRegA -> last (output $ runProgram program {regA = tryRegA}) == t) $
        map (upRegA .|.) [0 .. 7]
    newRegA = listToMaybe $ mapMaybe (findRegA program ts) newRegAOptions

part2 :: Program -> Maybe Int
part2 program@Program {code = code} = findRegA program (reverse code) 0

main :: IO ()
main = interact $ runner parser part1 part2
