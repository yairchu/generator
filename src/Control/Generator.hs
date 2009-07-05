{-# OPTIONS -O2 -Wall #-}

module Control.Generator(
  Producer, ConsumerT,
  append, empty, evalConsumerT, cons,
  mmerge, next, processRest, singleItem
  ) where

import Control.Monad (when)
import Control.Monad.Maybe (MaybeT (..))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.Maybe (fromMaybe, isNothing)

-- ConsProducer is like lists are for DList.
-- cons is O(1), but append and snoc are O(n)
newtype ConsProducer m v = ConsProducer { unConsProducer :: m (Maybe (v, ConsProducer m v)) }

-- Like DList
newtype Producer m v = Producer { unProducer :: ConsProducer m v -> ConsProducer m v }

singleItem :: Monad m => a -> Producer m a
singleItem a =
  Producer $ ConsProducer . return . Just . (,) a

append :: Monad m => Producer m a -> Producer m a -> Producer m a
append (Producer a) (Producer b) = Producer $ a . b

cons :: Monad m => a -> Producer m a -> Producer m a
cons = append . singleItem

mmerge :: Monad m => m (Producer m v) -> Producer m v
mmerge m =
  Producer $ \rest -> ConsProducer $ do
  a <- m
  unConsProducer $ unProducer a rest

empty :: Monad m => Producer m v
empty = Producer id

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

