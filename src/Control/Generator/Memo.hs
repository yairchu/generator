{-# OPTIONS -O2 -Wall #-}

module Control.Generator.Memo (memo) where

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Generator (Producer, mmerge)
import Control.Generator.Tools (transformProdMonad)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))

memoIO :: MonadIO m => m a -> IO (m a)
memoIO action = do
  ref <- newMVar Nothing
  return $ do
    x <- maybe action return =<< liftIO (takeMVar ref)
    liftIO . putMVar ref $ Just x
    return x

memo :: MonadIO m => Producer m v -> IO (Producer m v)
memo =
  liftM mmerge . memoIO .
  transformProdMonad (liftIO . memoIO)

