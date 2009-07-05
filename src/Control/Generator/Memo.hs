{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, Rank2Types, UndecidableInstances #-}

module Control.Generator.Memo (memo) where

import Control.Generator (Producer, mmerge)
import Control.Generator.Tools (transformProdMonad)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.MRef (
  newDefaultMRef, putDefaultMRef, takeDefaultMRef)
import Data.MRef.Classes (DefaultMRef, NewMRef, PutMRef, TakeMRef)

class (DefaultMRef r m a, NewMRef r m a, PutMRef r m a, TakeMRef r m a) => MRef r m a
instance (DefaultMRef r m a, NewMRef r m a, PutMRef r m a, TakeMRef r m a) => MRef r m a

memoM :: (Monad m, Monad s, MRef r m (Maybe a)) =>
  (forall x. m x -> s x) -> s a -> m (s a)
memoM liftFunc action = do
  ref <- newDefaultMRef Nothing
  return $ do
    x <- liftFunc $ takeDefaultMRef ref
    case x of
      Just res -> return res
      Nothing -> do
        res <- action
        liftFunc . putDefaultMRef ref $ Just res
        return res

memo :: (Monad m, Monad s, MRef r m (Maybe a)) =>
  (forall x. m x -> s x) -> Producer m a -> IO (Producer m b)
memo liftFunc =
  liftM mmerge . memoM liftFunc .
  transformProdMonad (liftIO . memoM liftFunc)

