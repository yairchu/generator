module Control.Generator.List (bfs, bfsLayers, dfs) where

import Control.Generator (
  Producer, evalConsumerT, next, processRest)
import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Data.Function (fix)
import Data.List (transpose)

dfs :: Producer [] a -> [a]
dfs =
  evalConsumerT . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> lift []
      Just x -> do
        r <- processRest rest
        lift (x : r)

bfsLayers :: Producer [] a -> [[a]]
bfsLayers prod =
  process . (`evalConsumerT` prod) . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> lift []
      Just x ->
        liftM (([x] :) . process) $
        processRest rest
  where
    process = map concat . transpose

bfs :: Producer [] a -> [a]
bfs = concat . bfsLayers

