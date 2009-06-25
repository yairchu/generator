{-# OPTIONS -O2 -Wall #-}

module Iterator.Tools (
  ifoldr, imap, itake
  ) where

import Iterator

import Control.Monad.Trans (lift)

ifoldr :: (Monad m) => (a -> m b -> m b) -> m b -> Iterator a m -> m b
ifoldr consFunc nilFunc iter =
  evalIteratesT (r =<< next) iter
  where
    r Nothing = lift nilFunc
    r (Just v) = do
      rest <- takeRest
      lift . consFunc v $ ifoldr consFunc nilFunc rest

imap :: Monad m => (a -> b) -> Iterator a m -> Iterator b m
imap func = iterator . ifoldr (cons . func) nil

itake :: (Monad m, Integral i) => i -> Iterator a m -> Iterator a m
itake 0 _ = iterator nil
itake count iter =
  iterator $ evalIteratesT (r =<< next) iter
  where
    r Nothing = lift nil
    r (Just v) = do
      rest <- takeRest
      lift . cons' v $ itake (count-1) rest

