{-# OPTIONS -O2 -Wall #-}

module Control.Generator.Consumer (
  ConsumerT, evalConsumerT, next, processRest
  ) where

import Control.Generator.Internal (ConsProducer(..), Producer(..))
import Control.Monad (when)
import Control.Monad.Maybe (MaybeT (..))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.Maybe (fromMaybe, isNothing)

newtype ConsumerT v m a = ConsumerT { unConsumerT :: StateT (Maybe (ConsProducer m v)) m a }

instance Monad m => Monad (ConsumerT v m) where
  return = ConsumerT . return
  fail = ConsumerT . fail
  (ConsumerT a) >>= b = ConsumerT $ a >>= unConsumerT . b

instance MonadTrans (ConsumerT v) where
  lift = ConsumerT . lift

instance MonadIO m => MonadIO (ConsumerT v m) where
  liftIO = lift . liftIO

emptyConsP :: Monad m => ConsProducer m a
emptyConsP = ConsProducer $ return Nothing

evalConsumerTConsP ::
  Monad m => ConsumerT v m a -> ConsProducer m v -> m a
evalConsumerTConsP (ConsumerT i) = evalStateT i . Just

evalConsumerT :: Monad m => ConsumerT v m a -> Producer m v -> m a
evalConsumerT i (Producer prod) = evalConsumerTConsP i $ prod emptyConsP

-- Consumer no longer has a producer left...
putNoProducer :: Monad m => StateT (Maybe (ConsProducer m v)) m ()
putNoProducer = put Nothing

next :: Monad m => ConsumerT v m (Maybe v)
next =
  ConsumerT . runMaybeT $ do
  ConsProducer prod <- MaybeT get
  (val, nextProducer) <- MaybeT $ do
    r <- lift prod
    when (isNothing r) putNoProducer
    return r
  lift $ putProducer nextProducer
  return val
  where
    putProducer = put . Just

processRest :: Monad m => ConsumerT a m b -> ConsumerT a m (m b)
processRest process =
  ConsumerT $ do
    mRest <- get
    let rest = fromMaybe emptyConsP mRest
    putNoProducer
    return $ evalConsumerTConsP process rest
