{-# OPTIONS -O2 -Wall #-}

module Control.Generator.ProducerT(
  ProducerT, produce, yield
  ) where

import Control.Generator (Producer, cons, empty, mmerge)
import Control.Monad (liftM)
import Control.Monad.Cont (Cont (..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))

newtype ProducerT v m a =
  ProducerT { unProducerT :: Cont (Producer m v) a }

instance Monad m => Monad (ProducerT v m) where
  return = ProducerT . return
  ProducerT a >>= f = ProducerT $ a >>= unProducerT . f
  fail = ProducerT . fail

instance MonadTrans (ProducerT v) where
  lift m = ProducerT . Cont $ \k -> mmerge . liftM k $ m

instance MonadIO m => MonadIO (ProducerT v m) where
  liftIO = lift . liftIO

yield :: Monad m => v -> ProducerT v m ()
yield v = ProducerT . Cont $ cons (return v) . ($ ())

produce :: Monad m => ProducerT v m () -> Producer m v
produce = ($ const empty) . runCont . unProducerT
