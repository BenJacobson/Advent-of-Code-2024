module Utils.List
  ( modifyAt,
    setAt,
  )
where

setAt :: [a] -> Int -> a -> [a]
setAt l i v = modifyAt l i (const v)

modifyAt :: [a] -> Int -> (a -> a) -> [a]
modifyAt [] _ _ = []
modifyAt (x : xs) 0 f = f x : xs
modifyAt (x : xs) i f = x : modifyAt xs (i - 1) f
