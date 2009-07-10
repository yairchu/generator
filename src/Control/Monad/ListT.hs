{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.ListT (
  List(..), ListItem(..), ListT(..)
) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Trans (MonadTrans(..))
import Data.List.Class (List(..), ListItem(..), cons, foldrL)
import Data.Monoid (Monoid(..))

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

instance Monad m => List (ListT m) m where
  joinL = ListT . (>>= runListT)
  unCons = runListT

