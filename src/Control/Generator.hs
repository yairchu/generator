{-# OPTIONS -O2 -Wall #-}

module Control.Generator (
  Producer, ConsumerT,
  empty, evalConsumerT, cons, cons',
  iterator, mmerge, next, nil, processRest
  ) where

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Maybe (fromMaybe)

type Producer' v m = m (Maybe (v, Producer v m))
newtype Producer v m = CProducer { unCProducer :: Producer' v m }

iterator :: Producer' v m -> Producer v m
iterator = CProducer

mmerge :: Monad m => m (Producer v m) -> Producer v m
mmerge mIter = iterator $ mIter >>= unCProducer

nil :: Monad m => Producer' v m
nil = return Nothing

empty :: Monad m => Producer v m
empty = iterator nil

cons'' :: Monad m => v -> Producer v m -> Producer' v m
cons'' v = return . Just . ((,) v)

cons :: Monad m => a -> Producer a m -> Producer a m
cons v = iterator . cons'' v

-- when constructing iterators
-- sometimes it's easier to do "cons' $ do"
cons' :: Monad m => v -> Producer' v m -> Producer' v m
cons' v = cons'' v . iterator

type ConsumerT' v m a = StateT (Maybe (Producer v m)) m a
newtype ConsumerT v m a = CConsumerT {
  uConsumerT :: ConsumerT' v m a
  }

instance Monad m => Monad (ConsumerT v m) where
  return = CConsumerT . return
  fail = CConsumerT . fail
  (CConsumerT a) >>= b = CConsumerT $ a >>= uConsumerT . b

instance MonadTrans (ConsumerT v) where
  lift = CConsumerT . lift

evalConsumerT :: Monad m => ConsumerT v m a -> Producer v m -> m a
evalConsumerT (CConsumerT i) = evalStateT i . Just

next :: Monad m => ConsumerT v m (Maybe v)
next =
  CConsumerT $ do
  x <- get
  case x of
    Nothing -> return Nothing
    Just (CProducer getNext) -> r =<< lift getNext
  where
    r Nothing = do
      put Nothing
      return Nothing
    r (Just (val, iter)) = do
      put $ Just iter
      return $ Just val

processRest :: Monad m => ConsumerT a m b -> ConsumerT a m (m b)
processRest process =
  CConsumerT $ do
  mRest <- get
  let rest = fromMaybe empty mRest
  put Nothing
  return $ evalConsumerT process rest

