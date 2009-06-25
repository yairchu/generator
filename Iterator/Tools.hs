module Iterator.Tools (
  imap
  ) where

import Iterator

import Control.Monad.Trans (lift)

imap :: Monad m => (a -> b) -> Iterator a m -> Iterator b m
imap func iter =
  iterator $ evalIteratesT r0 iter
  where
    r0 = do
      r1 =<< next
    r1 Nothing = lift nil
    r1 (Just v) = do
      rest <- takeRest
      lift . cons (func v) $ evalIteratesT r0 rest

