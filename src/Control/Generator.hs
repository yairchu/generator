{-# OPTIONS -O2 -Wall #-}

module Control.Generator(
  Producer, ConsumerT,
  empty, evalConsumerT, cons,
  mmerge, next, processRest
  ) where

import Control.Monad.State(StateT, evalStateT, get, put)
import Control.Monad.Trans(MonadTrans(..), MonadIO(..))
import Data.Maybe(fromMaybe)

newtype Producer v m = Producer { unProducer :: m (Maybe (v, Producer v m)) }

mmerge :: Monad m => m (Producer v m) -> Producer v m
mmerge mIter = Producer $ mIter >>= unProducer

empty :: Monad m => Producer v m
empty = Producer $ return Nothing

cons :: Monad m => a -> Producer a m -> Producer a m
cons v rest = Producer . return . Just $ (v, rest)

newtype ConsumerT v m a = ConsumerT { unConsumerT :: StateT (Maybe (Producer v m)) m a }

instance Monad m => Monad (ConsumerT v m) where
    return = ConsumerT . return
    fail = ConsumerT . fail
    (ConsumerT a) >>= b = ConsumerT $ a >>= unConsumerT . b

instance MonadTrans (ConsumerT v) where
    lift = ConsumerT . lift

instance MonadIO m => MonadIO (ConsumerT v m) where
    liftIO = lift . liftIO

evalConsumerT :: Monad m => ConsumerT v m a -> Producer v m -> m a
evalConsumerT (ConsumerT i) = evalStateT i . Just

-- Consumer moves on to this producer:
putProducer :: Monad m => Producer v m -> StateT (Maybe (Producer v m)) m ()
putProducer = put . Just
-- Consumer no longer has a producer left...
putNoProducer :: Monad m => StateT (Maybe (Producer v m)) m ()
putNoProducer = put Nothing

next :: Monad m => ConsumerT v m (Maybe v)
next = ConsumerT $ get >>= maybe -- no producer:
                                 (return Nothing)
                                 -- execute producer's action inside our inner monad:
                                 ((handleProducerResult =<<) . lift . unProducer)
  where
    handleProducerResult =
        maybe -- producer generated Nothing
              (putNoProducer >> return Nothing)
              -- producer generated a new item and continuation:
              producerItem
    producerItem (val, nextProducer) = putProducer nextProducer >> return (Just val)

processRest :: Monad m => ConsumerT a m b -> ConsumerT a m (m b)
processRest process = ConsumerT $ do
                        mRest <- get
                        let rest = fromMaybe empty mRest
                        putNoProducer
                        return $ evalConsumerT process rest
