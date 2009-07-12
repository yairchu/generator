{-# OPTIONS -O2 -Wall #-}

-- | A monad transformer for the [partial] consumption of 'List's.
-- The interface closely mimics iterators in languages such as Python.
module Control.Monad.Consumer (
  ConsumerT, evalConsumerT, next, consumeRestM
  ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.ListT (ListT(..), ListItem(..))
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.List.Class (List(..))
import Data.Maybe (fromMaybe)

-- | A monad tranformer for consuming 'List's.
newtype ConsumerT v m a = ConsumerT { runConsumerT :: StateT (Maybe (ListT m v)) m a }

instance Monad m => Monad (ConsumerT v m) where
  return = ConsumerT . return
  fail = ConsumerT . fail
  a >>= b = ConsumerT $ runConsumerT a >>= runConsumerT . b

instance MonadTrans (ConsumerT v) where
  lift = ConsumerT . lift

instance MonadIO m => MonadIO (ConsumerT v m) where
  liftIO = lift . liftIO

-- | Consume a 'ListT'
evalConsumerT :: List l m => ConsumerT v m a -> l v -> m a
evalConsumerT (ConsumerT i) = evalStateT i . Just . toListT

-- Consumer no longer has a producer left...
putNoProducer :: List l m => StateT (Maybe (l v)) m ()
putNoProducer = put Nothing

-- | Consume/get the next value
next :: Monad m => ConsumerT v m (Maybe v)
next =
  ConsumerT . runMaybeT $ do
  list <- MaybeT get
  item <- lift . lift $ runListT list
  case item of
    Nil -> do
      lift putNoProducer
      mzero
    Cons x xs -> do
      putProducer xs
      return x
  where
    putProducer = put . Just

-- | Return an instance of the underlying monad that will use the given 'ConsumerT' to consume the remaining values.
-- After this action there are no more items to consume (they belong to the given ConsumerT now)
consumeRestM :: List l m => ConsumerT a m b -> ConsumerT a m (m b)
consumeRestM consume =
  ConsumerT $ do
    mRest <- get
    let rest = fromMaybe mzero mRest
    putNoProducer
    return $ evalConsumerT consume rest

