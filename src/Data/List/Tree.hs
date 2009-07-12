{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-- | Functions for flattening 'Producer's of the lists.
-- A 'Producer []' is a tree.
module Data.List.Tree (
  Tree, dfs, bfs, bfsLayers, bestFirstSearchOn,
  prune, bestFirstSearchSortedChildrenOn
  ) where

import Control.Monad (MonadPlus(..), guard, join, liftM)
import Control.Monad.ListT (ListT(..), ListItem(..))
import Data.List.Class (
  List(..), cons, foldlL, fromList, toList,
  transformListMonad)

class (List l k, List k m) => Tree l k m
instance (List l k, List k m) => Tree l k m

search :: (List l m, MonadPlus m) => (m (m a) -> m a) -> l a -> m a
search merge =
  merge . foldrL step mzero
  where
    step a = return . cons a . merge

-- | Flatten a tree in DFS pre-order. (Depth First Search)
dfs :: (List l m, MonadPlus m) => l a -> m a
dfs = search join

transpose :: Monad m => ListT m (ListT m v) -> ListT m (ListT m v)
transpose matrix =
  joinL $ toList matrix >>= r
  where
    r = liftM t . mapM runListT
    t items =
      cons (fromList (map headL items)) .
      joinL . r $ map tailL items

toListTree :: Tree l k m => l a -> ListT (ListT m) a
toListTree = transformListMonad toListT

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: Tree l k m => l a -> k (k a)
bfsLayers =
  fromListT . liftM fromListT .
  search (liftM join . transpose) . liftM return .
  toListTree

-- | Flatten a tree in BFS order. (Breadth First Search)
bfs :: Tree l k m => l a -> k a
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
  (Ord b, Tree l k m) => (a -> b) -> l a -> k a
bestFirstSearchOn func =
  fromListT . search (mergeOn func) . toListTree

mergeOnSortedHeads ::
  (Ord b, Monad m) => (a -> b) -> ListT m (ListT m a) -> ListT m a
mergeOnSortedHeads f list =
  joinL $ do
    item <- runListT list
    case item of
      Nil -> return mzero
      Cons xx yys -> do
        xi <- runListT xx
        return $ case xi of
          Nil -> mergeOnSortedHeads f yys
          Cons x xs ->
            cons x . mergeOnSortedHeads f $ bury xs yys
  where
    bury xx yyy =
      joinL $ do
        xi <- runListT xx
        case xi of
          Nil -> return yyy
          Cons x xs -> bury' x xs yyy
    bury' x xs yyy = do
      yyi <- runListT yyy
      case yyi of
        Nil -> return . return $ cons x xs
        Cons yy yys -> do
          yi <- runListT yy
          case yi of
            Nil -> bury' x xs yys
            Cons y ys
              | f x <= f y -> return . cons (cons x xs) $ cons (cons y ys) yys
              | otherwise -> return . cons (cons y ys) =<< bury' x xs yys

-- | Best-First-Search given that a node's children are in sorted order (best first) and given a scoring function.
-- Especially useful for trees where nodes have an infinite amount of children, where 'bestFirstSearchOn' will get stuck.
bestFirstSearchSortedChildrenOn ::
  (Ord o, Tree l k m) => (a -> o) -> l a -> k a
bestFirstSearchSortedChildrenOn func =
  fromListT . search (mergeOnSortedHeads func) . toListTree

joinM :: List l m => l (m a) -> l a
joinM =
  joinL . foldrL consFunc (return mzero)
  where
    consFunc action rest = do
      x <- action
      return . cons x . joinL $ rest

prune :: Tree l k m => (a -> Bool) -> l a -> l a
prune cond =
  joinM . liftM r
  where
    r x = do
      guard $ cond x
      return x


