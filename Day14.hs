import Data.Set qualified as Set
import Debug.Trace (traceShow, traceShowId)
import Utils.AoC (runner)
import Utils.Grid2D (Pos)
import Utils.String (split)

type Input = (Pos, [(Pos, Pos)])

parsePos :: String -> Pos
parsePos rawInput = (i, j)
  where
    [i, j] = map read $ split (== ',') rawInput

parseRobot :: String -> (Pos, Pos)
parseRobot rawInput = (p, v)
  where
    [p, v] = map (parsePos . drop 2) $ split (== ' ') rawInput

parser :: String -> Input
parser rawInput = ((width, height), robots)
  where
    (size : rawRobots) = lines rawInput
    [width, height] = map read $ split (== ' ') size
    robots = map parseRobot rawRobots

moveRobot :: Int -> Pos -> (Pos, Pos) -> (Pos, Pos)
moveRobot seconds (width, height) ((x, y), (dx, dy)) = ((nx, ny), (dx, dy))
  where
    nx = (((x + (dx * seconds)) `mod` width) + width) `mod` width
    ny = (((y + (dy * seconds)) `mod` height) + height) `mod` height

moveRobots :: Input -> Int -> Input
moveRobots (size, robots) seconds = (size, map (moveRobot seconds size) robots)

safetyFactor :: Pos -> [Pos] -> Int
safetyFactor (width, height) robots = q1 * q2 * q3 * q4
  where
    midWidth = width `div` 2
    midheight = height `div` 2
    q1 = length $ filter (\(x, y) -> x > midWidth && y > midheight) robots
    q2 = length $ filter (\(x, y) -> x < midWidth && y > midheight) robots
    q3 = length $ filter (\(x, y) -> x < midWidth && y < midheight) robots
    q4 = length $ filter (\(x, y) -> x > midWidth && y < midheight) robots

part1 :: Input -> Int
part1 input@(size, _) = safetyFactor size $ map fst $ snd $ moveRobots input 100

printRobots :: Int -> Input -> Int
printRobots time ((width, height), robots) =
  traceShow (if interesting then map traceShowId grid else []) time
  where
    interesting = time `mod` height == 53
    robotsPos = Set.fromList $ map fst robots
    grid =
      show time
        : [ [ if Set.member (x, y) robotsPos then 'X' else ' '
              | x <- [0 .. (width - 1)]
            ]
            | y <- [0 .. (height - 1)]
          ]

searchForChristmasTree :: Input -> Int -> Int -> Int
searchForChristmasTree input@(size, robots) currentTime maxTime
  | currentTime == maxTime = 0
  | otherwise = searchForChristmasTree newInput (printRobots currentTime newInput + 1) maxTime
  where
    newInput = moveRobots input 1

part2 :: Input -> Int
part2 input = searchForChristmasTree input 1 10001

main :: IO ()
main = interact $ runner parser part1 part2
