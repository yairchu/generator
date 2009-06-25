{-# OPTIONS -O2 -Wall #-}

module Data.Iterator.Tools (
  ifoldr, ifoldl, imap, ifilter, itake, iTakeWhile
  ) where

import Data.Iterator

import Control.Monad.Trans (lift)

ifoldl :: (Monad m) => (a -> b -> a) -> a -> Iterator b m -> m a
ifoldl func startVal =
  evalIteratesT (r =<< next)
  where
    r Nothing = return startVal
    r (Just v) = do
      rest <- takeRest
      lift $ ifoldl func (func startVal v) rest

-- consFunc takes "m b" and not "b" so could avoid running the rest
ifoldr :: (Monad m) => (a -> m b -> m b) -> m b -> Iterator a m -> m b
ifoldr consFunc nilFunc =
  evalIteratesT (r =<< next)
  where
    r Nothing = lift nilFunc
    r (Just v) = do
      rest <- takeRest
      lift . consFunc v $ ifoldr consFunc nilFunc rest

imap :: Monad m => (a -> b) -> Iterator a m -> Iterator b m
imap func = iterator . ifoldr (cons . func) nil

ifilter :: Monad m => (a -> Bool) -> Iterator a m -> Iterator a m
ifilter func =
  iterator . ifoldr r nil
  where
    r x xs = if func x then cons x xs else xs

iTakeWhile :: Monad m => (a -> Bool) -> Iterator a m -> Iterator a m
iTakeWhile func =
  iterator . ifoldr r nil
  where
    r x xs = if func x then cons x xs else nil

itake :: (Monad m, Integral i) => i -> Iterator a m -> Iterator a m
itake 0 _ = iterator nil
itake count iter =
  iterator $ evalIteratesT (r =<< next) iter
  where
    r Nothing = lift nil
    r (Just v) = do
      rest <- takeRest
      lift . cons' v $ itake (count-1) rest

