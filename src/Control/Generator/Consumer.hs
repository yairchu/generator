{-# OPTIONS -O2 -Wall #-}

-- | Consumption of 'Producer's.
-- All consumption of Producers is done using 'evalConsumerT'

module Control.Generator.Consumer (
  ConsumerT, evalConsumerT, next, consumeRestM
  ) where

import Control.Generator.Internal (ConsProducer(..), Producer(..))
import Control.Monad (when)
import Control.Monad.Maybe (MaybeT (..))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.Maybe (fromMaybe, isNothing)

-- | A monad tranformer for consuming 'Producer's. To consume a 'Producer m a' one needs to use a 'ConsumerT a m' monad.
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

-- | Consume a 'Producer' with a 'ConsumerT'
evalConsumerT :: Monad m => ConsumerT v m a -> Producer m v -> m a
evalConsumerT i (Producer prod) = evalConsumerTConsP i $ prod emptyConsP

-- Consumer no longer has a producer left...
putNoProducer :: Monad m => StateT (Maybe (ConsProducer m v)) m ()
putNoProducer = put Nothing

-- | Consume next value
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

-- | Return an instance of the underlying monad that will use the given 'ConsumerT' to consume the remaining values.
-- After this action there are no more items to consume (they belong to the given ConsumerT now)
consumeRestM :: Monad m => ConsumerT a m b -> ConsumerT a m (m b)
consumeRestM consume =
  ConsumerT $ do
    mRest <- get
    let rest = fromMaybe emptyConsP mRest
    putNoProducer
    return $ evalConsumerTConsP consume rest

