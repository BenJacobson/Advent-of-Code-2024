module Utils.AoC
  ( runner,
  )
where

runner :: (Show a, Show b) => (String -> p) -> (p -> a) -> (p -> b) -> String -> String
runner parser part1 part2 inputStr =
  "Part 1: " ++ res1 ++ "\n" ++ "Part 2: " ++ res2
  where
    input = parser inputStr
    res1 = show $ part1 input
    res2 = show $ part2 input
