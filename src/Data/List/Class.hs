{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}

module Data.List.Class (
  -- | The List typeclass
  BaseList(..), FoldList(..), List (..), ListItem (..),
  -- | List operations for MonadPlus
  cons, fromList, filter,
  -- | Standard list operations for FoldList instances
  takeWhile, toList,
  -- | Standard list operations for List instances
  genericDrop, genericTake, genericLength,
  -- | Non standard List operations
  foldlL, listFoldrL, splitAtL, execute
  ) where

import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.Identity (Identity(..))
import Data.Foldable (Foldable, foldl')
import Prelude hiding (filter, takeWhile)

data ListItem l a =
  Nil |
  Cons { headL :: a, tailL :: l a }

class (MonadPlus l, Monad m) => BaseList l m | l -> m where
  joinL :: m (l b) -> l b

class BaseList l m => FoldList l m | l -> m where
  foldrL :: (a -> m b -> m b) -> m b -> l a -> m b

class BaseList l m => List l m | l -> m where
  unCons :: l a -> m (ListItem l a)

instance BaseList [] Identity where
  joinL = runIdentity

instance List [] Identity where
  unCons [] = Identity Nil
  unCons (x : xs) = Identity $ Cons x xs

instance FoldList [] Identity where
  foldrL = listFoldrL

cons :: MonadPlus m => a -> m a -> m a
cons = mplus . return

fromList :: (MonadPlus m, Foldable t) => t a -> m a
fromList = foldl' (flip (mplus . return)) mzero

filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter cond =
  (>>= f)
  where
    f x
      | cond x = return x
      | otherwise = mzero

-- for foldrL and foldlL
fold' :: List l m => (a -> l a -> m b) -> m b -> l a -> m b
fold' step nilFunc list = do
  item <- unCons list
  case item of
    Nil -> nilFunc
    Cons x xs -> step x xs

listFoldrL :: List l m => (a -> m b -> m b) -> m b -> l a -> m b
listFoldrL consFunc nilFunc = 
  fold' step nilFunc
  where
    step x = consFunc x . listFoldrL consFunc nilFunc

foldlL :: List l m => (a -> b -> a) -> a -> l b -> m a
foldlL step startVal =
  fold' step' $ return startVal
  where
    step' x = foldlL step $ step startVal x

takeWhile :: FoldList l m => (a -> Bool) -> l a -> l a
takeWhile cond =
  joinL . foldrL step (return mzero)
  where
    step x
      | cond x = return . cons x . joinL
      | otherwise = const $ return mzero

splitAtL :: (Integral i, List l m) => i -> l a -> m (l a, l a)
splitAtL count list
  | count <= 0 = return (mzero, list)
  | otherwise = do
    item <- unCons list
    case item of
      Nil -> return (mzero, mzero)
      Cons x xs -> do
        (pre, post) <- splitAtL (count - 1) xs
        return (cons x pre, post)

genericDrop :: (Integral i, List l m) => i -> l a -> l a
genericDrop count = joinL . liftM snd . splitAtL count

genericTake :: (Integral i, List l m) => i -> l a -> l a
genericTake count = joinL . liftM fst . splitAtL count

toList :: FoldList m i => m a -> i [a]
toList =
  foldrL step $ return []
  where
    step = liftM . (:)

genericLength :: (Integral i, List l m) => l a -> m i
genericLength = foldlL (const . (+ 1)) 0

execute :: FoldList l m => l a -> m ()
execute = foldrL (const id) $ return ()

