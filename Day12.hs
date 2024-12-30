import Data.List (sort, sortOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Utils.AoC (runner)
import Utils.Grid2D (Dir (..), Pos, allManhattanFrom, allPos, isInGrid, moveDir)

type Input = [[Char]]

parser :: String -> Input
parser = lines

constructRegion :: Input -> Char -> Pos -> Set Pos -> Set Pos
constructRegion input c pos@(i, j) region
  | not $ isInGrid input pos = region
  | input !! i !! j /= c = region
  | Set.member pos region = region
  | otherwise = foldr (constructRegion input c) addedRegion neighbors
  where
    neighbors = allManhattanFrom pos 1
    addedRegion = Set.insert pos region

checkRegion :: Input -> Pos -> ([Set Pos], Set Pos) -> ([Set Pos], Set Pos)
checkRegion input pos@(i, j) (ans, seen)
  | Set.member pos seen = (ans, seen)
  | otherwise = (region : ans, Set.union seen region)
  where
    region = constructRegion input (input !! i !! j) pos Set.empty

constructRegions :: Input -> [Set Pos]
constructRegions input = fst $ foldr (checkRegion input) ([], Set.empty) (allPos input)

countMissingNeighbors :: Set Pos -> Pos -> Int
countMissingNeighbors region pos = length $ filter (not . (`Set.member` region)) neighbors
  where
    neighbors = allManhattanFrom pos 1

calculatePerimeterPrice :: Set Pos -> Int
calculatePerimeterPrice region = area * perimeter
  where
    area = length region
    perimeter = foldr (\p acc -> countMissingNeighbors region p + acc) 0 region

part1 :: Input -> Int
part1 input = sum $ map calculatePerimeterPrice $ constructRegions input

fPlanks :: Set Pos -> (Pos -> Pos) -> Pos -> [Pos]
fPlanks region p pos = [pos | not $ Set.member (p pos) region]

areVerticalSiblings :: (Pos, Pos) -> Bool
areVerticalSiblings ((i1, j1), (i2, j2)) = j1 == j2 && (i1 + 1) == i2

areHorizontalSiblings :: (Pos, Pos) -> Bool
areHorizontalSiblings ((i1, j1), (i2, j2)) = i1 == i2 && (j1 + 1) == j2

leftSides :: Set Pos -> Int
leftSides region = sides
  where
    planks = sortOn (\(i, j) -> (j, i)) $ concatMap (fPlanks region (moveDir LEFT)) region
    sides = (+) 1 $ length $ filter (not . areVerticalSiblings) $ zip planks (tail planks)

rightSides :: Set Pos -> Int
rightSides region = sides
  where
    planks = sortOn (\(i, j) -> (j, i)) $ concatMap (fPlanks region (moveDir RIGHT)) region
    sides = (+) 1 $ length $ filter (not . areVerticalSiblings) $ zip planks (tail planks)

upSides :: Set Pos -> Int
upSides region = sides
  where
    planks = sort $ concatMap (fPlanks region (moveDir UP)) region
    sides = (+) 1 $ length $ filter (not . areHorizontalSiblings) $ zip planks (tail planks)

downSides :: Set Pos -> Int
downSides region = sides
  where
    planks = sort $ concatMap (fPlanks region (moveDir DOWN)) region
    sides = (+) 1 $ length $ filter (not . areHorizontalSiblings) $ zip planks (tail planks)

calculateSidesPrice :: Set Pos -> Int
calculateSidesPrice region = area * sides
  where
    area = length region
    sides = sum $ map ($ region) [leftSides, rightSides, upSides, downSides]

part2 :: Input -> Int
part2 input = sum $ map calculateSidesPrice $ constructRegions input

main :: IO ()
main = interact $ runner parser part1 part2
