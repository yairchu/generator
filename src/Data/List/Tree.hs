{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for flattening 'Producer's of the lists.
-- A 'Producer []' is a tree.
module Data.List.Tree (
  dfs, bfs, bfsLayers, bestFirstSearchOn,
  bestFirstSearchSortedChildrenOn
  ) where

import Control.Monad (liftM)
import Data.List (transpose)
import Data.List.Class (FoldList(..))

search :: FoldList l [] => ([[a]] -> [a]) -> l a -> [a]
search merge =
  merge . foldrL step []
  where
    step a = return . (a :) . merge

-- | Flatten a tree in DFS pre-order. (Depth First Search)
dfs :: FoldList l [] => l a -> [a]
dfs = search concat

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: FoldList l [] => l a -> [[a]]
bfsLayers = search (map concat . transpose) . liftM return

-- | Flatten a tree in BFS order. (Breadth First Search)
bfs :: FoldList l [] => l a -> [a]
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
bestFirstSearchOn ::
  (Ord o, FoldList l []) => (a -> o) -> l a -> [a]
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
  (Ord o, FoldList l []) => (a -> o) -> l a -> [a]
bestFirstSearchSortedChildrenOn =
  search . mergeOnSortedHeads

