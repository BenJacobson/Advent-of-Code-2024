module Utils.BFS (bfs) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

bfs ::
  (Ord vert, Ord cost) =>
  ((vert, cost) -> [(vert, cost)]) -> -- get adjacent vertices with their cost
  (vert -> Bool) -> -- whether to stop searching at this vertex
  Set (cost, vert, vert) -> -- an ordered list of (cost, next vertex, previous vertex)
  Map vert (cost, [vert]) -> -- map verticies to with their cost and optimal ancestors
  Map vert (cost, [vert]) -- the fully populated map of vertex costs and optimal paths
bfs adj isDest queue seen
  | Set.null queue = seen
  | Map.member vert seen = bfs adj isDest remainingQueue seenWithFromVert
  | isDest vert = seenWithNeighbors
  | otherwise = bfs adj isDest queueWithNeighbors seenWithNeighbors
  where
    ((cost, vert, fromVert), remainingQueue) = fromJust $ Set.minView queue
    seenWithFromVert = -- add additional optimal ancestors to existing seen vertices
      maybe
        seen
        ( \(oldCost, oldNeighbors) ->
            if oldCost == cost
              then Map.insert vert (oldCost, fromVert : oldNeighbors) seen
              else seen
        )
        (Map.lookup vert seen)
    neighbors = adj (vert, cost)
    seenHere = Map.insert vert (cost, [fromVert]) seen
    seenWithNeighbors =
      foldr (\(key, val) s -> Map.insert key val s) seenHere $
        mapMaybe
          ( \(v, newCost) ->
              Map.lookup v seenHere
                >>= ( \(oldCost, oldNeighbors) ->
                        if oldCost == newCost
                          then Just (v, (oldCost, vert : oldNeighbors))
                          else Nothing
                    )
          )
          neighbors
    -- only add search vertices that are still feasible
    newNeighbors = filter (not . (`Map.member` seenWithNeighbors) . fst) neighbors
    queueWithNeighbors = foldr (\(v, c) q -> Set.insert (c, v, vert) q) remainingQueue newNeighbors
