{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses, UndecidableInstances #-}

module Data.List.Class (
  -- | The List typeclass
  BaseList(..), FoldList(..), List (..), ListItem (..),
  -- | List operations for MonadPlus
  cons, fromList, filter,
  -- | Standard list operations for FoldList instances
  takeWhile, genericLength, scanl,
  -- | Standard list operations for List instances
  genericTake,
  -- | Non standard FoldList operations
  foldlL, execute, toList,
  -- | For implementing FoldList instances from List
  listFoldrL
  ) where

import Control.Monad (MonadPlus(..), ap, join, liftM)
import Control.Monad.Identity (Identity(..))
import Data.Foldable (Foldable, foldr)
import Prelude hiding (foldr, filter, takeWhile, scanl)

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
fromList = foldr (mplus . return) mzero

filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter cond =
  (>>= f)
  where
    f x
      | cond x = return x
      | otherwise = mzero

listFoldrL :: List l m => (a -> m b -> m b) -> m b -> l a -> m b
listFoldrL consFunc nilFunc list = do
  item <- unCons list
  case item of
    Nil -> nilFunc
    Cons x xs -> consFunc x $ listFoldrL consFunc nilFunc xs

-- for foldlL and scanl
foldlL' :: FoldList l m =>
  (a -> m c -> c) -> (a -> c) -> (a -> b -> a) -> a -> l b -> c
foldlL' joinVals atEnd step startVal =
  t startVal . foldrL astep (return atEnd)
  where
    astep x rest = return $ (`t` rest) . (`step` x)
    t cur = joinVals cur . (`ap` return cur)

foldlL :: FoldList l m => (a -> b -> a) -> a -> l b -> m a
foldlL step startVal =
  foldlL' (const join) id astep (return startVal)
  where
    astep rest x = liftM (`step` x) rest

scanl :: FoldList l m => (a -> b -> a) -> a -> l b -> l a
scanl =
  foldlL' consJoin $ const mzero
  where
    consJoin cur = cons cur . joinL

genericTake :: (Integral i, FoldList l m) => i -> l a -> l a
genericTake count
  | count <= 0 = const mzero
  | otherwise = foldlL' joinStep (const mzero) next Nothing
  where
    next Nothing x = Just (count, x)
    next (Just (i, _)) y = Just (i - 1, y)
    joinStep Nothing = joinL
    joinStep (Just (1, x)) = const $ return x
    joinStep (Just (_, x)) = cons x . joinL

execute :: FoldList l m => l (m ()) -> m ()
execute = foldrL (>>) (return ())

takeWhile :: FoldList l m => (a -> Bool) -> l a -> l a
takeWhile cond =
  joinL . foldrL step (return mzero)
  where
    step x
      | cond x = return . cons x . joinL
      | otherwise = const $ return mzero

toList :: FoldList m i => m a -> i [a]
toList =
  foldrL step $ return []
  where
    step = liftM . (:)

genericLength :: (Integral i, FoldList l m) => l a -> m i
genericLength = foldlL (const . (+ 1)) 0
