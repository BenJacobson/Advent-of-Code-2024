module Utils.String
  ( split,
    strip,
  )
where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

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
