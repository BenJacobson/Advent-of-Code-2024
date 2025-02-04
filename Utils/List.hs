module Utils.List
  ( modifyAt,
    setAt,
    subarrayIndex,
    setSubarray,
    removeOrderedDuplicates,
    count,
  )
where

setAt :: [a] -> Int -> a -> [a]
setAt l i v = modifyAt l i (const v)

modifyAt :: [a] -> Int -> (a -> a) -> [a]
modifyAt [] _ _ = []
modifyAt (x : xs) 0 f = f x : xs
modifyAt (x : xs) i f = x : modifyAt xs (i - 1) f

subarrayIndex :: (Eq a) => [a] -> [a] -> Int -> Maybe Int
subarrayIndex [] [] i = Just i
subarrayIndex [] _ _ = Nothing
subarrayIndex _ [] _ = Nothing
subarrayIndex arr sarr i
  | take (length sarr) arr == sarr = Just i
  | otherwise = subarrayIndex (tail arr) sarr (i + 1)

setSubarray :: [a] -> [a] -> Int -> [a]
setSubarray [] _ _ = []
setSubarray arr [] _ = arr
setSubarray (a : arr) (s : sarr) 0 = s : setSubarray arr sarr 0
setSubarray (a : arr) sarr i = a : setSubarray arr sarr (i - 1)

removeOrderedDuplicates :: (Eq a) => [a] -> [a]
removeOrderedDuplicates (x1 : x2 : xs) =
  if x1 == x2
    then removeOrderedDuplicates (x1 : xs)
    else x1 : removeOrderedDuplicates (x2 : xs)
removeOrderedDuplicates x = x

count :: (a -> Bool) -> [a] -> Int
count p xs = length [0 | x <- xs, p x]