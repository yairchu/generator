{-# OPTIONS -O2 -Wall #-}

module Control.Generator.Memo (memo) where

import Control.Generator (Producer, mmerge)
import Control.Generator.Tools (transformProdMonad)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.MRef (
  newDefaultMRef, putDefaultMRef, takeDefaultMRef)

memoIO :: MonadIO m => m a -> IO (m a)
memoIO action = do
  ref <- newDefaultMRef Nothing
  return $ do
    x <- liftIO $ takeDefaultMRef ref
    case x of
      Just res -> return res
      Nothing -> do
        res <- action
        liftIO . putDefaultMRef ref $ Just res
        return res

memo :: MonadIO m => Producer m v -> IO (Producer m v)
memo =
  liftM mmerge . memoIO .
  transformProdMonad (liftIO . memoIO)

