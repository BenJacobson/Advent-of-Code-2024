import Data.Map (Map)
import Data.Map qualified as Map
import Utils.AoC (runner)
import Utils.Grid2D (Dir (..), Pos, allPos, isInGrid, moveDir, setCell)
import Utils.String (split)

data Cell = BLOCK | BOX | BOX_LEFT | BOX_RIGHT | FREE
  deriving (Eq)

type Grid = [[Cell]]

type Input = (Pos, Grid, [Dir])

parseCell :: Char -> Cell
parseCell '#' = BLOCK
parseCell 'O' = BOX
parseCell '.' = FREE
parseCell '@' = FREE

parseMove :: Char -> Dir
parseMove '^' = UP
parseMove '<' = LEFT
parseMove '>' = RIGHT
parseMove 'v' = DOWN

parser :: String -> Input
parser rawInput = (startPos, grid, moves)
  where
    [rawGrid, rawMoves] = map lines $ split (== '$') rawInput
    startPos = head $ filter (\(i, j) -> rawGrid !! i !! j == '@') $ allPos rawGrid
    grid = map (map parseCell) rawGrid
    moves = map parseMove $ concat rawMoves

pushBoxes :: Grid -> Pos -> Dir -> Map Pos Cell -> Map Pos Cell
pushBoxes grid pos@(i, j) dir seen
  | Map.member pos seen = seen
  | cell == FREE = seen
  | cell == BLOCK = newSeen
  | cell == BOX_LEFT && (dir == UP || dir == DOWN) = continuePushWithRight
  | cell == BOX_RIGHT && (dir == UP || dir == DOWN) = continuePushWithLeft
  | otherwise = continuePush
  where
    cell = grid !! i !! j
    newPos = moveDir dir pos
    newSeen = Map.insert pos cell seen
    continuePush = pushBoxes grid newPos dir newSeen
    continuePushWithLeft = pushBoxes grid (moveDir LEFT pos) dir continuePush
    continuePushWithRight = pushBoxes grid (moveDir RIGHT pos) dir continuePush

gridFolder :: (Pos, Grid) -> Dir -> (Pos, Grid)
gridFolder (pos@(i, j), grid) dir =
  let newPos = moveDir dir pos
      pushMap = pushBoxes grid newPos dir (Map.singleton pos FREE)
      isBlocked = Map.foldr (\cell blocked -> cell == BLOCK || blocked) False pushMap
      resetGrid = foldr (\p g -> setCell g p FREE) grid (Map.keys pushMap)
      newGrid = foldr (\(p, c) g -> setCell g (moveDir dir p) c) resetGrid (Map.assocs pushMap)
   in if isBlocked then (pos, grid) else (newPos, newGrid)

scoreGrid :: Grid -> Int
scoreGrid grid =
  sum $
    map (\(i, j) -> 100 * i + j) $
      filter (\(i, j) -> grid !! i !! j == BOX || grid !! i !! j == BOX_LEFT) $
        allPos grid

solve :: Input -> Int
solve (pos, grid, moves) = scoreGrid finalGrid
  where
    (_, finalGrid) = foldl gridFolder (pos, grid) moves

part1 :: Input -> Int
part1 = solve

expandCell :: Cell -> [Cell]
expandCell FREE = [FREE, FREE]
expandCell BLOCK = [BLOCK, BLOCK]
expandCell BOX = [BOX_LEFT, BOX_RIGHT]

part2 :: Input -> Int
part2 input =
  solve (expandedStart, expandedGrid, moves)
  where
    ((i, j), grid, moves) = input
    expandedStart = (i, 2 * j)
    expandedGrid = map (concatMap expandCell) grid

main :: IO ()
main = interact $ runner parser part1 part2
