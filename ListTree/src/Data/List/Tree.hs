{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies, UndecidableInstances, ConstraintKinds #-}

-- | Functions for iterating trees.
-- A 'List' whose underlying monad is also a 'List' is a tree.
--
-- It's nodes are accessible, in contrast to the list monad,
-- which can also be seen as a tree, except only its leafs
-- are accessible and only in "dfs order".
--
-- > import Control.Monad.ListT.Funcs (repeatM)
-- > import Data.List.Class (genericTake, scanl, takeWhile, toList, lastL)
-- > import Prelude hiding (scanl, takeWhile)
-- >
-- > appendToEnd xs x = xs ++ [x]
-- > bits = scanl appendToEnd [] (repeatM "01")
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
  Tree, TreeT, TreeItemM,
  -- | Search algorithms
  dfs, bfs, bfsLayers,
  bestFirstSearchOn,
  bestFirstSearchSortedChildrenOn,
  sortChildrenOn,
  -- | Pruning methods
  prune, pruneM,
  branchAndBound
  ) where

import Control.Applicative (Alternative(..), liftA2)
import Control.Monad (MonadPlus(..), guard, join, when)
import Control.Monad.ListT (ListT(..))
import Control.Monad.Trans.State (StateT, get, put)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.List (sortOn)
import Data.List.Class (
  List(..), ListItem(..), cons, mapListItem,
  foldrL, fromList, mergeOn, transpose,
  toList, transformListMonad, joinL)
import Data.Maybe (isNothing)

type Tree t = (List t, List (ItemM t))
type TreeT m a = ListT (ListT m) a
type TreeItemM t = ItemM (ItemM t)

search :: Tree t => (ItemM t (ItemM t a) -> ItemM t a) -> t a -> ItemM t a
search merge =
  merge . foldrL step empty
  where
    step a = pure . cons a . merge

-- | Iterate a tree in DFS pre-order. (Depth First Search)
dfs :: Tree t => t a -> ItemM t a
dfs = search join

-- | Transform a tree into lists of the items in its different layers
bfsLayers :: Tree t => t a -> ItemM t (ItemM t a)
bfsLayers = search (fmap join . transpose) . fmap pure

-- | Iterate a tree in BFS order. (Breadth First Search)
bfs :: Tree t => t a -> ItemM t a
bfs = join . bfsLayers

-- | Best First Search given a scoring function.
bestFirstSearchOn ::
  (Ord b, Tree t) => (a -> b) -> t a -> ItemM t a
bestFirstSearchOn = search . mergeOn

mergeOnSortedHeads ::
  (Ord b, List l) => (a -> b) -> l (l a) -> l a
mergeOnSortedHeads f =
  -- naming convention for this func:
  -- fooh = head foo
  -- foot = tail foo
  -- foo_ = foo reconstructed after deconstruction
  -- (reconstructed so that monadic action isn't ran twice)
  mapListItem empty
  ( \h t ->
    mapListItem (mergeOnSortedHeads f t)
    (\hh ht -> cons hh . mergeOnSortedHeads f $ bury ht t)
    h
  )
  where
    mapRepack list onHead onTail =
      mapListItem onHead (\h t -> onTail (cons h t) h) list
    bury a b =
      mapRepack a b
      ( \a_ ah ->
        mapListItem (pure a_)
        ( \bh bt ->
          mapRepack bh (bury a_ bt)
          ( \bh_ bhh ->
            if f ah <= f bhh
            then cons a_ (cons bh_ bt)
            else cons bh_ (bury a_ bt)
          )
        ) b
      )

-- | Prune a tree or list given a predicate.
-- Unlike 'takeWhile' which stops a branch where the condition doesn't hold,
-- prune "cuts" the whole branch (the underlying MonadPlus's mzero).
prune :: MonadPlus m => (a -> Bool) -> ListT m a -> ListT m a
prune = pruneM . fmap pure

pruneM :: MonadPlus m => (a -> m Bool) -> ListT m a -> ListT m a
pruneM cond list = do
  x <- list
  lift (cond x >>= guard)
  pure x

-- | Best-First-Search given that a node's children are in sorted order (best first) and given a scoring function.
-- Especially useful for trees where nodes have an infinite amount of children, where 'bestFirstSearchOn' will get stuck.
--
-- Example: Find smallest Pythagorian Triplets
--
-- > pythagorianTriplets =
-- > pythagorianTriplets =
-- >   mapMaybe (^? _Right) .
-- >   bestFirstSearchSortedChildrenOn (^? _Left) .
-- >   generate $ do
-- >     x <- lift [1..]
-- >     yield (Left x)
-- >     y <- lift [x..]
-- >     yield (Left (x + y))
-- >     z <- lift [y..]
-- >     yield (Left (x + y + z))
-- >     guard $ x*x + y*y == z*z
-- >     yield (Right (x, y, z))
-- >
-- > > take 10 pythagorianTriplets
-- > [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20),(7,24,25),(10,24,26),(15,20,25),(20,21,29)]

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
  -> t a -> TreeT (StateT (Maybe b) (TreeItemM t)) a
branchAndBound boundFunc =
  pruneM cond . transformListMonad (transformListMonad lift)
  where
    cond x = do
      upperSoFar <- lift get
      if Just True == liftA2 (>=) lower upperSoFar
        then pure False
        else do
          -- this "when" clause isn't before the if,
          -- so upper bound won't be calculated if not needed.
          -- this optiminzation is based on (upper >= lower)
          when
            ( isNothing upperSoFar
            || Just True == liftA2 (<) upper upperSoFar
            ) (lift (put upper))
          pure True
      where
        (lower, upper) = boundFunc x

sortChildrenOn :: (Ord b, Tree t) => (a -> b) -> t a -> ListT (ItemM t) a
sortChildrenOn func =
  ListT . joinL . fmap (fromList . sortOn f) . toList . runList . transformListMonad id
  where
    f (Cons x _) = Just (func x)
    f _ = Nothing
