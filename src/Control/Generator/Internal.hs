{-# OPTIONS -O2 -Wall #-}
module Control.Generator.Internal (ConsProducer(..), Producer(..)) where

-- This module is for INTERNAL Generator use only (and is not
-- exported). If exported, it would allow forking of a Producer in the
-- middle (big nono).

-- ConsProducer is like lists are for DList.
-- cons is O(1), but append and snoc are O(n)
newtype ConsProducer m v = ConsProducer { unConsProducer :: m (Maybe (v, ConsProducer m v)) }

-- Like DList
newtype Producer m v = Producer { unProducer :: ConsProducer m v -> ConsProducer m v }
