module Control.Generator.List (
  dfs, bfs, bfsLayers, bestFirstSearch
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
bfsLayers = search (: []) (map concat . transpose)

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

