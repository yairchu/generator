{-# OPTIONS -O2 -Wall #-}

module Data.Iterator.Tools (
  append, fromList, ifoldr, ifoldl, imap,
  ifilter, itake, iTakeWhile, toList
  ) where

import Control.Monad (liftM)
import Control.Monad.Trans (lift)
import Data.Iterator (
  Iterator, cons, cons', empty, evalIteratesT,
  iterator, mmerge, next, nil, takeRest)

-- naming: for everything that's in prelude I add an "i" prefix,
-- for convinient importing

-- a strict foldl
ifoldl :: (Monad m) => (a -> b -> a) -> a -> Iterator b m -> m a
ifoldl func startVal =
  evalIteratesT (r =<< next)
  where
    r Nothing = return startVal
    r (Just v) = do
      rest <- takeRest
      let x = func startVal v
      seq x . lift $ ifoldl func x rest

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
imap func = iterator . ifoldr (cons' . func) nil

ifilter :: Monad m => (a -> Bool) -> Iterator a m -> Iterator a m
ifilter func =
  iterator . ifoldr r nil
  where
    r x = if func x then cons' x else id

-- uses ifoldl because I think with ifoldr it would use much mem, right?
toList :: (Monad m) => Iterator a m -> m [a]
toList = liftM reverse . ifoldl (flip (:)) []

iTakeWhile :: Monad m => (a -> Bool) -> Iterator a m -> Iterator a m
iTakeWhile func =
  iterator . ifoldr r nil
  where
    r x xs = if func x then cons' x xs else nil

fromList :: (Monad m) => [a] -> Iterator a m
fromList = iterator . foldr cons' nil

append :: Monad m => Iterator a m -> Iterator a m -> Iterator a m
append a b =
  mmerge $ ifoldr step (return b) a
  where
    step x = return . cons x . mmerge

itake :: (Monad m, Integral i) => i -> Iterator a m -> Iterator a m
itake 0 _ = empty
itake count iter =
  mmerge $ evalIteratesT (r =<< next) iter
  where
    r Nothing = return empty
    r (Just v) = do
      rest <- takeRest
      return . cons v $ itake (count-1) rest

