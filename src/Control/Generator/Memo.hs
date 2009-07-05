{-# OPTIONS -O2 -Wall #-}

module Control.Generator.Memo (memo) where

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Generator (Producer, mmerge)
import Control.Generator.Tools (transformProdMonad)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))

memoIO :: MonadIO m => m a -> IO (m a)
memoIO action = do
  ref <- liftIO $ newMVar Nothing
  return $ do
    x <- liftIO $ takeMVar ref
    case x of
      Just res -> return res
      Nothing -> do
        res <- action
        liftIO . putMVar ref $ Just res
        return res

memo :: MonadIO m => Producer m v -> IO (Producer m v)
memo =
  liftM mmerge . memoIO .
  transformProdMonad (liftIO . memoIO)

