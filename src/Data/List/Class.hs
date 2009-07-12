{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, RankNTypes, UndecidableInstances #-}

-- | The 'List' class and actions for lists

module Data.List.Class (
  -- | The List typeclass
  List (..),
  -- | List operations for MonadPlus
  cons, fromList, filter,
  -- | Standard list operations
  takeWhile, genericTake, scanl, sequence_,
  -- | Non standard List operations
  foldlL, toList, execute, lengthL,
  transformListMonad, convList, joinM
  ) where

import Control.Monad (MonadPlus(..), ap, join, liftM)
import Control.Monad.Identity (Identity(..))
import Control.Monad.ListT (ListT(..), foldrListT)
import Prelude hiding (filter, takeWhile, sequence_, scanl)

-- | A class for list types.
-- Every list has an underlying monad.
class (MonadPlus l, Monad m) => List l m | l -> m where
  -- | Transform an action returning a list to the returned list
  joinL :: m (l b) -> l b
  -- | foldr for 'List's.
  -- the result and "right" values are monadic actions.
  foldrL :: (a -> m b -> m b) -> m b -> l a -> m b
  foldrL consFunc nilFunc = foldrL consFunc nilFunc . toListT
  -- | Convert to a 'ListT'
  toListT :: l a -> ListT m a
  toListT = convList
  -- | Convert from a 'ListT'
  fromListT :: ListT m a -> l a
  fromListT = convList

instance List [] Identity where
  joinL = runIdentity
  foldrL = foldr
  toListT = fromList

instance Monad m => List (ListT m) m where
  joinL = ListT . (>>= runListT)
  foldrL = foldrListT
  toListT = id
  fromListT = id

-- | Prepend an item to a 'MonadPlus'
cons :: MonadPlus m => a -> m a -> m a
cons = mplus . return

-- | Convert a list to a 'MonadPlus'
fromList :: MonadPlus m => [a] -> m a
fromList = foldr (mplus . return) mzero

-- | Convert between list types
convList :: (List l m, List k m) => l a -> k a
convList =
  joinL . foldrL step (return mzero)
  where
    step x = return . cons x . joinL

filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter cond =
  (>>= f)
  where
    f x
      | cond x = return x
      | otherwise = mzero

-- for foldlL and scanl
foldlL' :: List l m =>
  (a -> m c -> c) -> (a -> c) -> (a -> b -> a) -> a -> l b -> c
foldlL' joinVals atEnd step startVal =
  t startVal . foldrL astep (return atEnd)
  where
    astep x rest = return $ (`t` rest) . (`step` x)
    t cur = joinVals cur . (`ap` return cur)

-- | An action to do foldl for 'List's
foldlL :: List l m => (a -> b -> a) -> a -> l b -> m a
foldlL step startVal =
  foldlL' (const join) id astep (return startVal)
  where
    astep rest x = liftM (`step` x) rest

scanl :: List l m => (a -> b -> a) -> a -> l b -> l a
scanl =
  foldlL' consJoin $ const mzero
  where
    consJoin cur = cons cur . joinL

genericTake :: (Integral i, List l m) => i -> l a -> l a
genericTake count
  | count <= 0 = const mzero
  | otherwise = foldlL' joinStep (const mzero) next Nothing
  where
    next Nothing x = Just (count, x)
    next (Just (i, _)) y = Just (i - 1, y)
    joinStep Nothing = joinL
    joinStep (Just (1, x)) = const $ return x
    joinStep (Just (_, x)) = cons x . joinL

-- | Execute the monadic actions in a 'List'
execute :: List l m => l a -> m ()
execute = foldlL const ()

-- | Transform a list of actions to a list of their results
joinM :: List l m => l (m a) -> l a
joinM =
  joinL . foldrL consFunc (return mzero)
  where
    consFunc action rest = do
      x <- action
      return . cons x . joinL $ rest

sequence_ :: List l m => l (m a) -> m ()
sequence_ = execute . joinM

takeWhile :: List l m => (a -> Bool) -> l a -> l a
takeWhile cond =
  joinL . foldrL step (return mzero)
  where
    step x
      | cond x = return . cons x . joinL
      | otherwise = const $ return mzero

-- | An action to transform a 'List' to a list
toList :: List l m => l a -> m [a]
toList =
  foldrL step $ return []
  where
    step = liftM . (:)

-- | Consume a list (execute its actions) and return its length
lengthL :: (Integral i, List l m) => l a -> m i
lengthL = foldlL (const . (+ 1)) 0

-- | Transform the underlying monad of a list given a way to transform the monad
transformListMonad :: (List l m, List k s) =>
  (forall x. m x -> s x) -> l a -> k a
transformListMonad trans =
  t . foldrL step (return mzero)
  where
    t = joinL . trans
    step x = return . cons x . t

