{-# OPTIONS -O2 -Wall #-}

-- | A monad transformer for the creation of 'Producer's.
-- Similar to Python's generators.

module Control.Generator.ProducerT (
  ProducerT, produce, yield, yields
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Cont (Cont (..))
import Control.Monad.Producer (Producer, append, cons, empty, joinP)
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
  lift m = ProducerT . Cont $ \k -> joinP $ liftM k m

instance MonadIO m => MonadIO (ProducerT v m) where
  liftIO = lift . liftIO

-- | /O(1)/, Transform a ProducerT to a 'Producer'
produce :: Monad m => ProducerT v m () -> Producer m v
produce = ($ const empty) . runCont . unProducerT

-- | /O(1)/, Output a result value
yield :: Monad m => v -> ProducerT v m ()
yield x = ProducerT . Cont $ cons x . ($ ())

-- | /O(1)/, Output all the values of another 'Producer'.
yields :: Monad m => Producer m v -> ProducerT v m ()
yields xs = ProducerT . Cont $ append xs . ($ ())

