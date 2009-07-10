{-# OPTIONS -O2 -Wall #-}

module Control.Monad.Consumer (
  ConsumerT, evalConsumerTList, next, consumeRestM
  ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.ListT (ListT)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.List.Class (ListItem(..), List(..))
import Data.Maybe (fromMaybe)

-- | A monad tranformer for consuming 'Producer's. To consume a 'Producer m a' one needs to use a 'ConsumerT a m' monad.
newtype ConsumerT v m a = ConsumerT { unConsumerT :: StateT (Maybe (ListT m v)) m a }

instance Monad m => Monad (ConsumerT v m) where
  return = ConsumerT . return
  fail = ConsumerT . fail
  (ConsumerT a) >>= b = ConsumerT $ a >>= unConsumerT . b

instance MonadTrans (ConsumerT v) where
  lift = ConsumerT . lift

instance MonadIO m => MonadIO (ConsumerT v m) where
  liftIO = lift . liftIO

evalConsumerTList ::
  Monad m => ConsumerT v m a -> ListT m v -> m a
evalConsumerTList (ConsumerT i) = evalStateT i . Just

-- Consumer no longer has a producer left...
putNoProducer :: Monad m => StateT (Maybe (ListT m v)) m ()
putNoProducer = put Nothing

-- | Consume next value
next :: Monad m => ConsumerT v m (Maybe v)
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
consumeRestM :: Monad m => ConsumerT a m b -> ConsumerT a m (m b)
consumeRestM consume =
  ConsumerT $ do
    mRest <- get
    let rest = fromMaybe mzero mRest
    putNoProducer
    return $ evalConsumerTList consume rest


