{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -O2 -Wall #-}

-- | 'Producer's are like lists of values interleaved with monadic actions
-- (but the monadic actions can determine the values).
-- Only a 'ConsumerT' can get the values out of a 'Producer',
-- and can only consume the values in the correct order,
-- it's iteration executes the interleaved monadic actions.
module Control.Monad.Producer (
  Producer, consM,
  ConsumerT, evalConsumerT, next, consumeRestM
  ) where

import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.ListT (ListT)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.DList.Generic (DList(..), toList)
import Data.List.Class (BaseList(..), List(..), ListItem(..), cons)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..))

data Producer m a = Producer { runProducer :: DList (ListT m) a }

instance Monad m => Monoid (Producer m a) where
  mempty = Producer mempty
  mappend (Producer a) (Producer b) = Producer $ mappend a b

instance Monad m => Functor (Producer m) where
  fmap func = Producer . fmap func . runProducer

instance Monad m => Monad (Producer m) where
  return = Producer . return
  a >>= b = Producer $ runProducer a >>= liftM runProducer b

instance Monad m => MonadPlus (Producer m) where
  mzero = mempty
  mplus = mappend

instance Monad m => BaseList (Producer m) m where
  joinL action =
    Producer $ DList r
    where
      r rest = joinL $ liftM ((`runDList` rest) . runProducer) action

instance MonadTrans Producer where
  lift = (`consM` mzero)

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

consM :: Monad m => m a -> Producer m a -> Producer m a
consM action xs =
  joinL $ liftM (`cons` xs) action

evalConsumerTList ::
  Monad m => ConsumerT v m a -> ListT m v -> m a
evalConsumerTList (ConsumerT i) = evalStateT i . Just

-- | Consume a 'Producer' with a 'ConsumerT'
evalConsumerT :: Monad m => ConsumerT v m a -> Producer m v -> m a
evalConsumerT consumer = evalConsumerTList consumer . toList . runProducer

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

