{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}

-- | Functions for iterating trees.
-- A 'List' whose underlying monad is also a 'List' is a tree.
--
-- It's nodes are accessible, in contrast to the list monad,
-- which can also be seen as a tree, except only its leafs
-- are accessible and only in "dfs order".
--
-- > import Control.Monad.Generator
-- > import Control.Monad.Trans
-- > import Data.List.Class (genericTake, takeWhile, toList, lastL)
-- >
-- > bits = generate (t "")
-- > t prev = do
-- >   yield prev
-- >   x <- lift "01"
-- >   t (prev ++ [x])
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
  Tree,
  -- | Search algorithms
  dfs, bfs, bfsLayers,
  bestFirstSearchOn,
  bestFirstSearchSortedChildrenOn,
  sortChildrenOn,
  -- | Pruning methods
  prune, pruneM,
  branchAndBound
  ) where

import Control.Monad (
  MonadPlus(..), guard, join, liftM, liftM2, when)
import Control.Monad.ListT (ListT(..))
import Control.Monad.Trans.State (StateT, get, put)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.List.Class (
  List(..), ListItem(..), cons,
  foldrL, fromList, mergeOn, transpose,
  sortOn, toList, transformListMonad)

-- | A 'type-class synonym' for Trees.
class (List t, List (ItemM t)) => Tree t
instance (List t, List (ItemM t)) => Tree t

search :: (List l, MonadPlus (ItemM l)) =>
  (ItemM l (ItemM l a) -> ItemM l a) -> l a -> ItemM l a
search merge =
  merge . foldrL step mzero
  where
    step a = return . cons a . merge

-- | Iterate a tree in DFS pre-order. (Depth First Search)
dfs :: (List l, MonadPlus (ItemM l)) => l a -> ItemM l a
dfs = search join

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: Tree t => t a -> ItemM t (ItemM t a)
bfsLayers =
  search (liftM join . transpose) . liftM return

-- | Iterate a tree in BFS order. (Breadth First Search)
bfs :: Tree t => t a -> ItemM t a
bfs = join . bfsLayers

-- | Best First Search given a scoring function.
bestFirstSearchOn ::
  (Ord b, Tree t) => (a -> b) -> t a -> ItemM t a
bestFirstSearchOn = search . mergeOn

mergeOnSortedHeads ::
  (Ord b, List l) => (a -> b) -> l (l a) -> l a
mergeOnSortedHeads f ll =
  joinL $ do
    lli <- runList ll
    case lli of
      Nil -> return mzero
      Cons l ls -> do
        li <- runList l
        return $ case li of
          Nil -> mergeOnSortedHeads f ls
          Cons x xs ->
            cons x . mergeOnSortedHeads f $ bury xs ls
  where
    bury xx yyy =
      joinL $ do
        xi <- runList xx
        case xi of
          Nil -> return yyy
          Cons x xs -> do
            let rxx = cons x xs
            yyi <- runList yyy
            case yyi of
              Nil -> return (cons rxx mzero)
              Cons yy yys -> do
                yi <- runList yy
                return $ case yi of
                  Nil -> bury xx yys
                  Cons y ys ->
                    let ryy = cons y ys
                    in if f x <= f y
                      then cons rxx . cons ryy $ yys
                      else cons ryy . bury rxx $ yys

-- | Prune a tree or list given a predicate.
-- Unlike 'takeWhile' which stops a branch where the condition doesn't hold,
-- prune "cuts" the whole branch (the underlying MonadPlus's mzero).
prune :: MonadPlus m => (a -> Bool) -> ListT m a -> ListT m a
prune = pruneM . fmap return

pruneM :: MonadPlus m => (a -> m Bool) -> ListT m a -> ListT m a
pruneM cond list = do
  x <- list
  lift (cond x >>= guard)
  return x
  
-- | Best-First-Search given that a node's children are in sorted order (best first) and given a scoring function.
-- Especially useful for trees where nodes have an infinite amount of children, where 'bestFirstSearchOn' will get stuck.
--
-- Example: Find smallest Pythagorian Triplets
--
-- > import Control.Monad
-- > import Control.Monad.DList (toListT)
-- > import Control.Monad.Generator
-- > import Control.Monad.Trans
-- > import Data.List.Tree
-- > import Data.Maybe
-- >
-- > pythagorianTriplets =
-- >   catMaybes .
-- >   fmap fst .
-- >   bestFirstSearchSortedChildrenOn snd .
-- >   toListT . generate $ do
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
bestFirstSearchSortedChildrenOn =
  search . mergeOnSortedHeads

-- | Generalized "Branch and Bound". A method for pruning.
--
-- The result of this function
-- would usually be given to another search algorithm,
-- such as `dfs`, in order to find the node with lowest value.
--
-- This augments the regular search by pruning the tree.
-- Given a function to calculate a lower and upper bound for a subtree,
-- we keep the lowest upper bound (hence the State monad) encountered so far,
-- and we prune any subtree whose lower bound is over the known upper bound.
branchAndBound ::
  (Ord b, Tree t) => (a -> (Maybe b, Maybe b))
  -> t a -> ListT (ListT (StateT (Maybe b) (ItemM (ItemM t)))) a
branchAndBound boundFunc =
  pruneM cond . transformListMonad (transformListMonad lift)
  where
    cond x = do
      upperSoFar <- lift get
      if Just True == liftM2 (>=) lower upperSoFar
        then return False
        else do
          -- this "when" clause isn't before the if,
          -- so upper bound won't be calculated if not needed.
          -- this optiminzation is based on (upper >= lower)
          when
            ( Nothing == upperSoFar
            || Just True == liftM2 (<) upper upperSoFar
            ) (lift (put upper))
          return True
      where
        (lower, upper) = boundFunc x

sortChildrenOn ::
  forall a b t. (Ord b, Tree t) => (a -> b)
  -> t a -> ListT (ItemM t) a
sortChildrenOn func =
  ListT . joinL . liftM (fromList . sortOn f) . toList . runList . transformListMonad id
  where
    f (Cons x _) = Just (func x)
    f _ = Nothing
