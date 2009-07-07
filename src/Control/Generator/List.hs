{-# OPTIONS -O2 -Wall #-}

module Control.Generator.List (
  dfs, bfs, bfsLayers, bestFirstSearch,
  prune, bestFirstSearchSortedChildren
  ) where

import Control.Generator (
  Producer, evalConsumerT, next, processRest)
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Data.Function (fix)
import Data.List (transpose)

search :: (a -> b) -> ([[b]] -> [b]) -> Producer [] a -> [b]
search trans merge prod =
  merge . (`evalConsumerT` prod) . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> lift []
      Just x ->
        liftM ((trans x :) . merge) $ processRest rest

dfs :: Producer [] a -> [a]
dfs = search id concat

bfsLayers :: Producer [] a -> [[a]]
bfsLayers = search return (map concat . transpose)

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

bestFirstSearch :: Ord n => Producer [] (a, n) -> [a]
bestFirstSearch = map fst . search id (mergeOn snd)

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

bestFirstSearchSortedChildren ::
  Ord n => Producer [] (a, n) -> [a]
bestFirstSearchSortedChildren =
  map fst . search id (mergeOnSortedHeads snd)

