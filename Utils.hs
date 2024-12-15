module Utils
  ( Dir (..),
    allPos,
    freqMap,
    fromTupleList,
    isInGrid,
    isInRange,
    moveDir,
    turnRight,
    setAt,
    modifyAt,
    isSpace,
    split,
    strip,
  )
where

import Data.List
import Data.Map (Map, empty, insertWith)

data Dir = UP | DOWN | LEFT | RIGHT
  deriving (Show, Ord, Eq)

turnRight :: Dir -> Dir
turnRight UP = RIGHT
turnRight RIGHT = DOWN
turnRight DOWN = LEFT
turnRight LEFT = UP

moveDir :: Dir -> (Int, Int) -> (Int, Int)
moveDir UP (i, j) = (i - 1, j)
moveDir RIGHT (i, j) = (i, j + 1)
moveDir DOWN (i, j) = (i + 1, j)
moveDir LEFT (i, j) = (i, j - 1)

isInRange :: Int -> Int -> Int -> Bool
isInRange l h v = l <= v && v <= h

isInGrid :: [[a]] -> (Int, Int) -> Bool
isInGrid [] _ = False
isInGrid (g : gs) (i, j) = isInRange 0 (length gs) i && isInRange 0 (length g - 1) j

allPos :: [[a]] -> [(Int, Int)]
allpos [] = []
allPos (r : rs) = [(i, j) | i <- [0 .. length rs], j <- [0 .. length r - 1]]

freqMap :: (Ord a) => [a] -> Map a Int
freqMap = foldr (\x m -> insertWith (+) x 1 m) empty

fromTupleList :: (Ord a) => [(a, b)] -> Map a [b]
fromTupleList = foldr (\(a, b) m -> insertWith (++) a [b] m) empty

setAt :: [a] -> Int -> a -> [a]
setAt l i v = modifyAt l i (const v)

modifyAt :: [a] -> Int -> (a -> a) -> [a]
modifyAt [] _ _ = []
modifyAt (x : xs) 0 f = f x : xs
modifyAt (x : xs) i f = x : modifyAt xs (i - 1) f

isSpace :: Char -> Bool
isSpace c
  | c == ' ' = True
  | c == '\n' = True
  | otherwise = False

strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

split :: (Char -> Bool) -> String -> [String]
split p s = filter (/= "") $ map strip $ go p s
  where
    go _ [] = [""]
    go d (c : s)
      | p c = "" : h : t
      | otherwise = (c : h) : t
      where
        (h : t) = go p s
