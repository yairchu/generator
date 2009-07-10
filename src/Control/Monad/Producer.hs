{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- | 'Producer's are like lists of values interleaved with monadic actions
-- (but the monadic actions can determine the values).
-- Only a 'ConsumerT' can get the values out of a 'Producer',
-- and can only consume the values in the correct order,
-- it's iteration executes the interleaved monadic actions.
module Control.Monad.Producer (
  Producer, consume, consM
  ) where

import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.Consumer (ConsumerT, evalConsumerT)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans (MonadTrans(..))
import Data.DList.Generic (DList(..), toList)
import Data.List.Class (BaseList(..), cons)
import Data.Monoid (Monoid(..))

data Producer m a = Producer { runProducer :: DList (ListT m) a }

instance Monad m => Monoid (Producer m a) where
  mempty = Producer mempty
  mappend (Producer a) (Producer b) = Producer $ mappend a b

instance Monad m => Functor (Producer m) where
  fmap func = Producer . fmap func . runProducer

instance Monad m => Monad (Producer m) where
  return = Producer . return
  a >>= b = Producer $ runProducer a >>= liftM runProducer b

instance Monad m => MonadPlus (Producer m) where
  mzero = mempty
  mplus = mappend

instance Monad m => BaseList (Producer m) m where
  joinL action =
    Producer $ DList r
    where
      r rest = joinL $ liftM ((`runDList` rest) . runProducer) action

instance MonadTrans Producer where
  lift = (`consM` mzero)

consM :: Monad m => m a -> Producer m a -> Producer m a
consM action xs =
  joinL $ liftM (`cons` xs) action

-- | Consume a 'Producer' with a 'ConsumerT'
consume :: Monad m => ConsumerT v m a -> Producer m v -> m a
consume consumer = evalConsumerT consumer . toList . runProducer

