{-# LANGUAGE FlexibleInstances, FunctionalDependencies, MultiParamTypeClasses #-}

module Control.Monad.ListT (
  List(..), ListItem(..), ListT(..),
  cons, fromList, filter, takeWhile,
  foldrL, foldlL
) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Foldable (Foldable, foldl')
import Data.Monoid (Monoid(..))

import Prelude hiding (filter, takeWhile)

data ListItem l a =
  Nil |
  Cons { headT :: a, tailT :: l a }

data ListT m a = ListT { runListT :: m (ListItem (ListT m) a) }

-- for mappend, fmap, bind
foldrL' :: Monad m =>
  (a -> ListT m b -> ListT m b) -> ListT m b -> ListT m a -> ListT m b
foldrL' consFunc nilFunc =
  ListT . foldrL step (runListT nilFunc)
  where
    step x = runListT . consFunc x . ListT

instance Monad m => Monoid (ListT m a) where
  mempty = ListT $ return Nil
  mappend = flip (foldrL' cons)

instance Monad m => Functor (ListT m) where
  fmap func = foldrL' (cons . func) mempty

instance Monad m => Applicative (ListT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return = ListT . return . (`Cons` mempty)
  a >>= b = foldrL' mappend mempty $ fmap b a

instance Monad m => MonadPlus (ListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans ListT where
  lift = ListT . (>> return Nil)

cons :: MonadPlus m => a -> m a -> m a
cons = mplus . return

fromList :: (MonadPlus m, Foldable t) => t a -> m a
fromList = foldl' (flip (mplus . return)) mzero

class (MonadPlus m, Monad i) => List m i | m -> i where
  joinL :: i (m b) -> m b
  unCons :: m a -> i (ListItem m a)

instance Monad m => List (ListT m) m where
  joinL = ListT . (>>= runListT)
  unCons = runListT

instance List [] Identity where
  joinL = runIdentity
  unCons [] = Identity Nil
  unCons (x : xs) = Identity $ Cons x xs

-- for foldrL and foldlL
fold' :: List m i => (a -> m a -> i b) -> i b -> m a -> i b
fold' step nilFunc list = do
  item <- unCons list
  case item of
    Nil -> nilFunc
    Cons x xs -> step x xs

foldrL :: List m i => (a -> i b -> i b) -> i b -> m a -> i b
foldrL consFunc nilFunc = 
  fold' step nilFunc
  where
    step x = consFunc x . foldrL consFunc nilFunc

foldlL :: List m i => (a -> b -> a) -> a -> m b -> i a
foldlL step startVal =
  fold' step' $ return startVal
  where
    step' x = foldlL step $ step startVal x

-- for filter and takeWhile
filter' :: List m i => (i (m a) -> i (m a)) -> (a -> Bool) -> m a -> m a
filter' falseFunc cond =
  joinL . foldrL step (return mzero)
  where
    step x xs
      | cond x = return . cons x $ joinL xs
      | otherwise = falseFunc xs

filter :: List m i => (a -> Bool) -> m a -> m a
filter = filter' id

takeWhile :: List m i => (a -> Bool) -> m a -> m a
takeWhile = filter' . const $ return mzero

