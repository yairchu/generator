{-# OPTIONS -O2 -Wall #-}

-- | Memoize 'Producer's of IO so that their results will be saved and later consumptions will not execute the IO actions required to generate the values.

module Control.Generator.Memo (memo) where

import Control.Concurrent.MVar (newMVar, putMVar, takeMVar)
import Control.Monad.Producer (Producer)
import Control.Generator.Folds (transformProdMonad)
import Control.Monad (liftM)
import Control.Monad.Trans (MonadIO(..))
import Data.List.Class (joinL)

memoIO :: MonadIO m => m a -> IO (m a)
memoIO action = do
  ref <- newMVar Nothing
  return $ do
    x <- maybe action return =<< liftIO (takeMVar ref)
    liftIO . putMVar ref $ Just x
    return x

-- | Memoize a 'Producer IO'
memo :: MonadIO m => Producer m v -> IO (Producer m v)
memo =
  liftM joinL . memoIO .
  transformProdMonad (liftIO . memoIO)

