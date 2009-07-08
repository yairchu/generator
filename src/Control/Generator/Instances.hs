{-# OPTIONS -O2 -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Generator.Instances () where

import Control.Generator (Producer)
import Control.Generator.Tools (imap)

instance Monad m => Functor (Producer m) where
  fmap = imap . (return .)
