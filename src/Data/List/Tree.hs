{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Functions for flattening 'Producer's of the lists.
-- A 'Producer []' is a tree.
module Data.List.Tree (
  dfs, bfs, bfsLayers, bestFirstSearchOn,
  bestFirstSearchSortedChildrenOn
  ) where

import Control.Monad (MonadPlus(..), join, liftM)
import Control.Monad.ListT (ListT(..), ListItem(..))
import Data.List.Class (
  List(..), cons, foldlL, fromList, toList, convList)

search :: (List l m, MonadPlus m) => (m (m a) -> m a) -> l a -> m a
search merge =
  merge . foldrL step mzero
  where
    step a = return . cons a . merge

-- | Flatten a tree in DFS pre-order. (Depth First Search)
dfs :: (List l m, MonadPlus m) => l a -> m a
dfs = search join

transpose :: List l m => l (l v) -> l (l v)
transpose matrix =
  joinL $ toList (liftM toListT matrix) >>= r
  where
    r = liftM t . mapM runListT
    t items =
      cons (fromList (map headL items)) .
      joinL . r $ map tailL items

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: (List l k, List k m) => l a -> k (k a)
bfsLayers = convList . search (liftM join . transpose) . liftM return

-- | Flatten a tree in BFS order. (Breadth First Search)
bfs :: (List l k, List k m) => l a -> k a
bfs = join . bfsLayers

mergeOn :: (Ord b, Monad m) => (a -> b) -> ListT m (ListT m a) -> ListT m a
mergeOn f =
  joinL . foldlL merge2 mzero
  where
    merge2 xx yy =
      joinL $ do
        xi <- runListT xx
        yi <- runListT yy
        return $ case (xi, yi) of
          (Cons x xs, Cons y ys)
            | f y > f x -> cons x . merge2 xs $ cons y ys
            | otherwise -> cons y $ merge2 (cons x xs) ys
          (x, y) -> mplus (t x) (t y)
    t Nil = mzero
    t (Cons x xs) = cons x xs

-- | Best First Search given a scoring function.
bestFirstSearchOn ::
  (Ord b, Monad m) => (a -> b) -> ListT (ListT m) a -> ListT m a
bestFirstSearchOn = search . mergeOn

mergeOnSortedHeads ::
  Ord b => (a -> b) -> [[a]] -> [a]
mergeOnSortedHeads f list = undefined f list
{-
  joinL $ do
    item <- unCons list
    case item of
      Nil -> return mzero
      Cons xx yys -> do
        xi <- unCons xx
        return $ case xi of
          Nil -> mergeOnSortedHeads f yys
          Cons x xs ->
            cons x . mergeOnSortedHeads f $ bury xs yys
  where
    -- need to translate this part
    bury a ([] : b) = bury a b
    bury (a : as) ((b : bs) : bss)
      | f a <= f b = (a : as) : (b : bs) : bss
      | otherwise = (b : bs) : bury (a : as) bss
    bury a b = a : b -- one of them is []
-}

-- | Best-First-Search given that a node's children are in sorted order (best first) and given a scoring function.
-- Especially useful for trees where nodes have an infinite amount of children, where 'bestFirstSearchOn' will get stuck.
bestFirstSearchSortedChildrenOn ::
  (Ord o, List l []) => (a -> o) -> l a -> [a]
bestFirstSearchSortedChildrenOn =
  search . mergeOnSortedHeads

