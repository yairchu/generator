{-# OPTIONS -O2 -Wall #-}

module Control.Generator.Producer (
  Producer, append, empty, cons, joinP
  ) where

import Control.Generator.Internal (ConsProducer(..), Producer(..))

empty :: Monad m => Producer m v
empty = Producer id

append :: Monad m => Producer m a -> Producer m a -> Producer m a
append (Producer a) (Producer b) = Producer $ a . b

singleItem :: Monad m => a -> Producer m a
singleItem a =
  Producer $ ConsProducer . return . Just . (,) a

cons :: Monad m => a -> Producer m a -> Producer m a
cons = append . singleItem

joinP :: Monad m => m (Producer m v) -> Producer m v
joinP m =
  Producer $ \rest -> ConsProducer $ do
  a <- m
  unConsProducer $ unProducer a rest
