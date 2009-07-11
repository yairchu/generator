{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Control.Monad.DList (
  DListT (..), toList, consume
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), liftM, ap)
import Control.Monad.Consumer (ConsumerT, evalConsumerT)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans (MonadTrans(..))
import Data.List.Class (List(..), cons)
import Data.Monoid (Monoid(..))

newtype DListT m a = DListT { runDListT :: ListT m a -> ListT m a }

toList :: Monad m => DListT m a -> ListT m a
toList = (`runDListT` mzero)

instance Monoid (DListT l a) where
  mempty = DListT id
  mappend (DListT a) (DListT b) = DListT $ a . b

instance Monad m => Functor (DListT m) where
  fmap func = DListT . mplus . liftM func . toList

instance Monad m => Monad (DListT m) where
  return = DListT . cons
  a >>= b = DListT . mplus $ toList a >>= liftM toList b

instance Monad m => Applicative (DListT m) where
  pure = return
  (<*>) = ap

instance Monad m => MonadPlus (DListT m) where
  mzero = mempty
  mplus = mappend

instance Monad m => List (DListT m) m where
  joinL = DListT . mplus . joinL . liftM toList
  foldrL consFunc nilFunc =
    foldrL consFunc nilFunc . (`runDListT` mzero)

instance MonadTrans DListT where
  lift = DListT . mappend . lift

consume :: Monad m => ConsumerT a m b -> DListT m a -> m b
consume c = evalConsumerT c . toList

