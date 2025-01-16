module Utils.MaximalCliques (maximalCliques) where

import Data.List (mapAccumL)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

maximalCliques :: (Ord a) => Map a (Set a) -> [Set a]
maximalCliques graph = go Set.empty (Set.fromList (Map.keys graph)) Set.empty
  where
    pickpivot p x = head $ Set.elems p ++ Set.elems x
    go r p x
      | Set.null p && Set.null x = [r]
      | otherwise = concat . snd $ mapAccumL step' (p, x) $ Set.elems (p Set.\\ pivotNeighbors)
      where
        pivot = pickpivot p x
        pivotNeighbors = Map.findWithDefault Set.empty pivot graph
        step' (p', x') v =
          ( (Set.delete v p', Set.insert v x'),
            go (Set.insert v r) (Set.intersection nv p') (Set.intersection nv x')
          )
          where
            nv = Map.findWithDefault Set.empty v graph
