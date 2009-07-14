{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

-- | Functions for iterating trees.
-- A 'List' whose underlying monad is also a 'List' is a tree.
--
-- It's nodes are accessible, in contrast to the list monad,
-- which can also be seen as a tree, except only its leafs
-- are accessible and only in "dfs order".
--
-- > import Control.Monad.Generator
-- > import Data.List.Class (genericTake, takeWhile, toList, lastL)
-- >
-- > bits = t ""
-- > t prev =
-- >   generate $ do
-- >     yield prev
-- >     x <- lift "01"
-- >     yields $ t (prev ++ [x])
-- >
-- > > take 3 (bfsLayers bits)
-- > [[""],["0","1"],["00","01","10","11"]]
-- >
-- > > take 10 (bfs bits)
-- > ["","0","1","00","01","10","11","000","001","010"]
-- >
-- > > dfs (genericTake 4 bits)
-- > ["","0","00","000","001","01","010","011","1","10","100","101","11","110","111"]
-- >
-- > > toList $ genericTake 3 bits
-- > [["","0","00"],["","0","01"],["","1","10"],["","1","11"]]
--
-- Examples of pruning with 'prune' and 'takeWhile':
--
-- > > dfs . takeWhile (not . isSuffixOf "11") $ genericTake 4 bits
-- > ["","0","00","000","001","01","010","1","10","100","101"]
-- >
-- > > lastL . takeWhile (not . isSuffixOf "11") $ genericTake 4 bits
-- > ["000","001","010","01","100","101","1"]
-- >
-- > > lastL . prune (not . isSuffixOf "11") $ genericTake 4 bits
-- > ["000","001","010","100","101"]
--
module Data.List.Tree (
  Tree, dfs, bfs, bfsLayers, bestFirstSearchOn,
  prune, bestFirstSearchSortedChildrenOn
  ) where

import Control.Monad (MonadPlus(..), guard, join, liftM)
import Control.Monad.ListT (ListT(..), ListItem(..))
import Data.List.Class (
  List(..), cons, foldlL, sequence,
  transformListMonad, transpose)
import Prelude hiding (sequence)

-- | A 'type-class synonym' for Trees.
class (List l k, List k m) => Tree l k m
instance (List l k, List k m) => Tree l k m

search :: (List l m, MonadPlus m) => (m (m a) -> m a) -> l a -> m a
search merge =
  merge . foldrL step mzero
  where
    step a = return . cons a . merge

-- | Iterate a tree in DFS pre-order. (Depth First Search)
dfs :: (List l m, MonadPlus m) => l a -> m a
dfs = search join

toListTree :: Tree l k m => l a -> ListT (ListT m) a
toListTree = transformListMonad toListT

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: Tree l k m => l a -> k (k a)
bfsLayers =
  fromListT . liftM fromListT .
  search (liftM join . transpose) . liftM return .
  toListTree

-- | Iterate a tree in BFS order. (Breadth First Search)
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
  (Ord b, Tree l k m) => (a -> b) -> l a -> k a
bestFirstSearchSortedChildrenOn func =
  fromListT . search (mergeOnSortedHeads func) . toListTree

-- | Prune a tree or list given a predicate.
-- Unlike 'takeWhile' which stops a branch where the condition doesn't hold,
-- prune "cuts" the whole branch (the underlying MonadPlus's mzero).
prune :: (List l m, MonadPlus m) => (a -> Bool) -> l a -> l a
prune cond =
  joinL . sequence . liftM r
  where
    r x = do
      guard $ cond x
      return x

