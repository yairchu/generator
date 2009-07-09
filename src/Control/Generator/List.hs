{-# OPTIONS -O2 -Wall #-}

-- | Functions for flattening 'Producer's of the list monad.
-- A 'Producer []' is a tree.
module Control.Generator.List (
  dfs, bfs, bfsLayers, bestFirstSearchOn,
  prune, bestFirstSearchSortedChildrenOn
  ) where

import Control.Generator.Producer (Producer)
import Control.Generator.Instances ()
import Control.Generator.Tools (mapMP, foldrP)
import Data.List (transpose)

search :: ([[a]] -> [a]) -> Producer [] a -> [a]
search merge =
  merge . foldrP step []
  where
    step a = return . (a :) . merge

-- | Flatten a tree in DFS pre-order. (Depth First Search)
dfs :: Producer [] a -> [a]
dfs = search concat

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: Producer [] a -> [[a]]
bfsLayers = search (map concat . transpose) . fmap return

-- | Flatten a tree in BFS order. (Breadth First Search)
bfs :: Producer [] a -> [a]
bfs = concat . bfsLayers

mergeOn :: Ord b => (a -> b) -> [[a]] -> [a]
mergeOn f =
  foldl merge2 []
  where
    merge2 (x:xs) (y:ys)
      | f y > f x = x : merge2 xs (y:ys)
      | otherwise = y : merge2 (x:xs) ys
    merge2 xs ys = xs ++ ys

-- | Best First Search given a scoring function.
bestFirstSearchOn :: Ord o => (a -> o) -> Producer [] a -> [a]
bestFirstSearchOn = search . mergeOn

mergeOnSortedHeads :: Ord b => (a -> b) -> [[a]] -> [a]
mergeOnSortedHeads _ [] = []
mergeOnSortedHeads f ([] : xs) = mergeOnSortedHeads f xs
mergeOnSortedHeads f ((x : xs) : ys) =
  x : mergeOnSortedHeads f (bury xs ys)
  where
    bury a ([] : b) = bury a b
    bury (a : as) ((b : bs) : bss)
      | f a <= f b = (a : as) : (b : bs) : bss
      | otherwise = (b : bs) : bury (a : as) bss
    bury a b = a : b -- one of them is []

-- | Best-First-Search given that a node's children are in sorted order (best first) and given a scoring function.
-- Especially useful for trees where nodes have an infinite amount of children, where 'bestFirstSearchOn' will get stuck.
bestFirstSearchSortedChildrenOn ::
  Ord o => (a -> o) -> Producer [] a -> [a]
bestFirstSearchSortedChildrenOn =
  search . mergeOnSortedHeads

-- | Prune all tree paths where an item does not satisfy a condition.
-- Unlike takeWhileP, which stops each path where the condition was first unsatisfied, `prune` eliminates the whole path.
prune :: (a -> Bool) -> Producer [] a -> Producer [] a
prune cond = mapMP (filter cond . return)

