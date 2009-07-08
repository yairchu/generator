{-# OPTIONS -O2 -Wall #-}

module Control.Generator.List (
  dfs, bfs, bfsLayers, bestFirstSearchOn,
  prune, bestFirstSearchSortedChildrenOn
  ) where

import Control.Generator (
  Producer, evalConsumerT, next, processRest)
import Control.Generator.Instances ()
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Data.Function (fix)
import Data.List (transpose)

search :: ([[a]] -> [a]) -> Producer [] a -> [a]
search merge prod =
  merge . (`evalConsumerT` prod) . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> lift []
      Just x ->
        liftM ((x :) . merge) $ processRest rest

dfs :: Producer [] a -> [a]
dfs = search concat

bfsLayers :: Producer [] a -> [[a]]
bfsLayers = search (map concat . transpose) . fmap return

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

bestFirstSearchOn :: Ord o => (a -> o) -> Producer [] a -> [a]
bestFirstSearchOn = search . mergeOn

prune :: (a -> Bool) -> a -> [a]
prune cond x = [x | cond x]

mergeOnSortedHeads :: Ord b => (a -> b) -> [[a]] -> [a]
mergeOnSortedHeads _ [] = []
mergeOnSortedHeads f ([] : xs) = mergeOnSortedHeads f xs
mergeOnSortedHeads f ((x : xs) : ys) =
  x : mergeOnSortedHeads f (burry xs ys)
  where
    burry a ([] : b) = burry a b
    burry (a : as) ((b : bs) : bss)
      | f a <= f b = (a : as) : (b : bs) : bss
      | otherwise = (b : bs) : burry (a : as) bss
    burry a b = a : b -- one of them is []

bestFirstSearchSortedChildrenOn ::
  Ord o => (a -> o) -> Producer [] a -> [a]
bestFirstSearchSortedChildrenOn =
  search . mergeOnSortedHeads
