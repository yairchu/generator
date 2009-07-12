{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-- | Functions for flattening 'Producer's of the lists.
-- A 'Producer []' is a tree.
module Data.List.Tree (
  Tree, dfs, bfs, bfsLayers, bestFirstSearchOn,
  bestFirstSearchSortedChildrenOn
  ) where

import Control.Monad (MonadPlus(..), join, liftM)
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
        yyi <- runListT yyy
        case yyi of
          Nil -> return $ return xx
          Cons yy yys -> do
            yi <- runListT yy
            case yi of
              Nil -> return $ bury xx yys
              Cons y ys -> do
                let
                  sameYs = cons y ys
                  sameYys = cons sameYs yys
                xi <- runListT xx
                return $ case xi of
                  Nil -> sameYys
                  Cons x xs
                    | f x <= f y -> cons (cons x xs) sameYys
                    | otherwise -> cons sameYs $ bury (cons x xs) yys

-- | Best-First-Search given that a node's children are in sorted order (best first) and given a scoring function.
-- Especially useful for trees where nodes have an infinite amount of children, where 'bestFirstSearchOn' will get stuck.
bestFirstSearchSortedChildrenOn ::
  (Ord o, Tree l k m) => (a -> o) -> l a -> k a
bestFirstSearchSortedChildrenOn func =
  fromListT . search (mergeOnSortedHeads func) . toListTree

