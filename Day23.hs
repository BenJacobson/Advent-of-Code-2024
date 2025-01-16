import Data.List (intercalate, maximumBy, sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Utils.AoC (runner)
import Utils.MaximalCliques (maximalCliques)
import Utils.String (split)

type Input = Map String (Set String)

parseEdge :: String -> Input -> Input
parseEdge rawEdge m = m''
  where
    [u, v] = split (== '-') rawEdge
    m' = Map.insertWith Set.union u (Set.singleton v) m
    m'' = Map.insertWith Set.union v (Set.singleton u) m'

parser :: String -> Input
parser rawInput = foldr parseEdge Map.empty (lines rawInput)

connect3 :: Input -> String -> Set [String]
connect3 m v = Set.fromList connectedNeighbors
  where
    neighbors = Set.toList $ Map.findWithDefault Set.empty v m
    connected x y = Set.member y $ Map.findWithDefault Set.empty x m
    connectedNeighbors = [sort [v, u1, u2] | u1 <- neighbors, u2 <- neighbors, connected u1 u2]

part1 :: Input -> Int
part1 input =
  length $
    Set.unions $
      map (connect3 input) $
        filter ((== 't') . head) $
          Map.keys input

compareCliques :: Set a -> Set a -> Ordering
compareCliques a b
  | la < lb = LT
  | la > lb = GT
  | otherwise = EQ
  where
    la = length a
    lb = length b

part2 :: Input -> String
part2 input = intercalate "," (Set.toAscList maxClique)
  where
    connected x y = Set.member y $ Map.findWithDefault Set.empty x input
    allMaxCliques = maximalCliques input
    maxClique = maximumBy compareCliques allMaxCliques

main :: IO ()
main = interact $ runner parser part1 part2
