{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}

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
  List(..), cons, foldlL, joinM,
  transformListMonad, transpose)

-- | A 'type-class synonym' for Trees.
class (List t, List (ItemM t)) => Tree t
instance (List t, List (ItemM t)) => Tree t

search :: (List l, MonadPlus (ItemM l)) => (ItemM l (ItemM l a) -> ItemM l a) -> l a -> ItemM l a
search merge =
  merge . foldrL step mzero
  where
    step a = return . cons a . merge

-- | Iterate a tree in DFS pre-order. (Depth First Search)
dfs :: (List l, MonadPlus (ItemM l)) => l a -> ItemM l a
dfs = search join

toListTree :: Tree t => t a -> ListT (ListT (ItemM (ItemM t))) a
toListTree = transformListMonad toListT

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: Tree t => t a -> ItemM t (ItemM t a)
bfsLayers =
  fromListT . liftM fromListT .
  search (liftM join . transpose) . liftM return .
  toListTree

-- | Iterate a tree in BFS order. (Breadth First Search)
bfs :: Tree t => t a -> ItemM t a
bfs = join . bfsLayers

mergeOn ::
  forall a b m. (Ord b, Monad m) =>
  (a -> b) -> ListT m (ListT m a) -> ListT m a
mergeOn f =
  joinL . foldlL merge2 mzero
  where
    merge2 :: ListT m a -> ListT m a -> ListT m a
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
  (Ord b, Tree t) => (a -> b) -> t a -> ItemM t a
bestFirstSearchOn func =
  fromListT . search (mergeOn func) . toListTree

mergeOnSortedHeads ::
  forall a b m. (Ord b, Monad m) =>
  (a -> b) -> ListT m (ListT m a) -> ListT m a
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
    bury :: ListT m a -> ListT m (ListT m a) -> ListT m (ListT m a)
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
--
-- Example: Find smallest Pythagorian Triplets
--
-- > import Control.Monad
-- > import Control.Monad.Generator
-- > import Control.Monad.Trans
-- > import Data.List.Tree
-- > import Data.Maybe
-- >
-- > pythagorianTriplets =
-- >   catMaybes .
-- >   fmap fst .
-- >   bestFirstSearchSortedChildrenOn snd .
-- >   generate $ do
-- >     x <- lift [1..]
-- >     yield (Nothing, x)
-- >     y <- lift [1..]
-- >     yield (Nothing, x + y)
-- >     z <- lift [1..]
-- >     yield (Nothing, x + y + z)
-- >     lift . guard $ x^2 + y^2 == z^2
-- >     yield (Just (x, y, z), 0)
-- >
-- > > print $ take 10 pythagorianTriplets
-- > [(3,4,5),(4,3,5),(6,8,10),(8,6,10),(5,12,13),(12,5,13),(9,12,15),(12,9,15),(15,8,17),(8,15,17)]

bestFirstSearchSortedChildrenOn ::
  (Ord b, Tree t) => (a -> b) -> t a -> ItemM t a
bestFirstSearchSortedChildrenOn func =
  fromListT . search (mergeOnSortedHeads func) . toListTree

-- | Prune a tree or list given a predicate.
-- Unlike 'takeWhile' which stops a branch where the condition doesn't hold,
-- prune "cuts" the whole branch (the underlying MonadPlus's mzero).
prune :: (List l, MonadPlus (ItemM l)) => (a -> Bool) -> l a -> l a
prune cond =
  joinM . liftM r
  where
    r x = do
      guard $ cond x
      return x

