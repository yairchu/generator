{-# OPTIONS -O2 -Wall #-}

module Control.Generator.ProducerT (
  ProducerT, produce, yield, yields
  ) where

import Control.Generator (Producer, append, cons, empty, mmerge)
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Cont (Cont (..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))

newtype ProducerT v m a =
  ProducerT { unProducerT :: Cont (Producer m v) a }

instance Monad m => Functor (ProducerT v m) where
  fmap = liftM
instance Monad m => Applicative (ProducerT v m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ProducerT v m) where
  return = ProducerT . return
  ProducerT a >>= f = ProducerT $ a >>= unProducerT . f
  fail = lift . fail

instance MonadTrans (ProducerT v) where
  lift m = ProducerT . Cont $ \k -> mmerge . liftM k $ m

instance MonadIO m => MonadIO (ProducerT v m) where
  liftIO = lift . liftIO

yield :: Monad m => v -> ProducerT v m ()
yield x = ProducerT . Cont $ cons x . ($ ())

yields :: Monad m => Producer m v -> ProducerT v m ()
yields xs = ProducerT . Cont $ append xs . ($ ())

produce :: Monad m => ProducerT v m () -> Producer m v
produce = ($ const empty) . runCont . unProducerT
