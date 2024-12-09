module AoC
(
    runner
) where

import Debug.Trace

runner :: (Show a) => (String -> p) -> (p -> a) -> (p -> a) -> (String -> String)
runner parser part1 part2 inputStr = do
    let input = parser inputStr
    let res1 = show $ part1 input
    let res2 = show $ part2 input
    "Part 1: " ++ res1 ++ "\n" ++ "Part 2: " ++ res2
