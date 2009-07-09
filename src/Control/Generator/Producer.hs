{-# OPTIONS -O2 -Wall #-}

-- | 'Producer's are like lists of values interleaved with monadic actions
-- (but the monadic actions can determine the values).
-- Only a 'ConsumerT' can get the values out of a 'Producer',
-- and can only consume the values in the correct order,
-- it's iteration executes the interleaved monadic actions.
module Control.Generator.Producer (
  Producer, append, empty, cons, consM, joinP
  ) where

import Control.Generator.Internal (ConsProducer(..), Producer(..))
import Control.Monad (liftM)

-- | An empty producer
empty :: Monad m => Producer m v
empty = Producer id

-- | /O(1)/, Append two producer
append :: Monad m => Producer m a -> Producer m a -> Producer m a
append (Producer a) (Producer b) = Producer $ a . b

singleItem :: Monad m => a -> Producer m a
singleItem a =
  Producer $ ConsProducer . return . Just . (,) a

-- | /O(1)/, Prepend a single element to a producer
cons :: Monad m => a -> Producer m a -> Producer m a
cons = append . singleItem

-- | /O(1)/, Transform an "m (Producer m v)" to the producer it returns
joinP :: Monad m => m (Producer m v) -> Producer m v
joinP m =
  Producer $ \rest -> ConsProducer $ do
  a <- m
  unConsProducer $ unProducer a rest

singleItemM :: Monad m => m a -> Producer m a
singleItemM = joinP . liftM (`cons` empty)

-- | /O(1)/, Prepend a single element returned by a monadic action to a producer
consM :: Monad m => m a -> Producer m a -> Producer m a
consM = append . singleItemM

