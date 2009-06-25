{-# OPTIONS -O2 -Wall #-}

module Iterator.Tools (
  imap, itake
  ) where

import Iterator

import Control.Monad.Trans (lift)

imap :: Monad m => (a -> b) -> Iterator a m -> Iterator b m
imap func iter =
  iterator $ evalIteratesT (r =<< next) iter
  where
    r Nothing = lift nil
    r (Just v) = do
      rest <- takeRest
      lift . cons' (func v) $ imap func rest

itake :: (Monad m, Integral i) => i -> Iterator a m -> Iterator a m
itake 0 _ = iterator nil
itake count iter =
  iterator $ evalIteratesT (r =<< next) iter
  where
    r Nothing = lift nil
    r (Just v) = do
      rest <- takeRest
      lift . cons' v $ itake (count-1) rest

