{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Generator.Instances () where

import Control.Generator.Producer (Producer, empty, append)
import Control.Generator.Tools (imap)
import Data.Monoid (Monoid(..))

instance Monad m => Monoid (Producer m a) where
  mempty = empty
  mappend = append

instance Monad m => Functor (Producer m) where
  fmap = imap . (return .)
