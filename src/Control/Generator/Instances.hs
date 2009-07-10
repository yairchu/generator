{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Monoid, Functor, Monad, and MonadPlus instances for Producer
module Control.Generator.Instances () where

import Control.Generator.Folds (foldrP', mapMP)
import Control.Monad.Producer (
  Producer, cons, consM, empty, append)
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Monoid (Monoid(..))

instance Monad m => Monoid (Producer m a) where
  mempty = empty
  mappend = append

instance Monad m => Functor (Producer m) where
  fmap = mapMP . (return .)

instance Monad m => Monad (Producer m) where
  return = (`cons` empty)
  a >>= b = foldrP' append empty $ fmap b a

instance Monad m => MonadPlus (Producer m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans Producer where
  lift = (`consM` empty)

