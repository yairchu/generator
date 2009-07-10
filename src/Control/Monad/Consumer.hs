{-# OPTIONS -O2 -Wall #-}

module Control.Monad.Consumer (
  ConsumerT, evalConsumerT, next, consumeRestM
  ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.ListT (ListT)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.List.Class (ListItem(..), List(..))
import Data.Maybe (fromMaybe)

-- | A monad tranformer for [partially] consuming 'ListT's.
newtype ConsumerT v l m a = ConsumerT { runConsumerT :: StateT (Maybe (l v)) m a }

instance Monad m => Monad (ConsumerT v l m) where
  return = ConsumerT . return
  fail = ConsumerT . fail
  a >>= b = ConsumerT $ runConsumerT a >>= runConsumerT . b

instance MonadTrans (ConsumerT v l) where
  lift = ConsumerT . lift

instance MonadIO m => MonadIO (ConsumerT v l m) where
  liftIO = lift . liftIO

evalConsumerT ::
  List l m => ConsumerT v l m a -> l v -> m a
evalConsumerT (ConsumerT i) = evalStateT i . Just

-- Consumer no longer has a producer left...
putNoProducer :: List l m => StateT (Maybe (l v)) m ()
putNoProducer = put Nothing

-- | Consume next value
next :: List l m => ConsumerT v l m (Maybe v)
next =
  ConsumerT . runMaybeT $ do
  prod <- MaybeT get
  item <- lift . lift $ unCons prod
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
consumeRestM :: List l m => ConsumerT a l m b -> ConsumerT a l m (m b)
consumeRestM consume =
  ConsumerT $ do
    mRest <- get
    let rest = fromMaybe mzero mRest
    putNoProducer
    return $ evalConsumerT consume rest

