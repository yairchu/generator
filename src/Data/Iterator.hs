{-# OPTIONS -O2 -Wall #-}

-- Should it be Data.Iterator, Control.Iterator, or something else?

module Data.Iterator (
  Iterator, IteratesT,
  empty, evalIteratesT, cons, cons',
  iterator, mmerge, next, nil, processRest
  ) where

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Maybe (fromMaybe)

type Iterator' v m = m (Maybe (v, Iterator v m))
newtype Iterator v m = CIterator (Iterator' v m)

iterator :: Iterator' v m -> Iterator v m
iterator = CIterator

mmerge :: Monad m => m (Iterator v m) -> Iterator v m
mmerge mIter =
  iterator $ do
  CIterator iter <- mIter
  iter

nil :: Monad m => Iterator' v m
nil = return Nothing

empty :: Monad m => Iterator v m
empty = iterator nil

cons'' :: Monad m => v -> Iterator v m -> Iterator' v m
cons'' v = return . Just . ((,) v)

cons :: Monad m => a -> Iterator a m -> Iterator a m
cons v = iterator . cons'' v

cons' :: Monad m => v -> Iterator' v m -> Iterator' v m
cons' v = cons'' v . iterator

type IteratesT' v m a = StateT (Maybe (Iterator v m)) m a
newtype IteratesT v m a = CIteratesT {
  uIteratesT :: IteratesT' v m a
  }

instance Monad m => Monad (IteratesT v m) where
  return = CIteratesT . return
  fail = CIteratesT . fail
  (CIteratesT a) >>= b = CIteratesT $ a >>= uIteratesT . b

instance MonadTrans (IteratesT v) where
  lift = CIteratesT . lift

evalIteratesT :: Monad m => IteratesT v m a -> Iterator v m -> m a
evalIteratesT (CIteratesT i) = evalStateT i . Just

next :: Monad m => IteratesT v m (Maybe v)
next =
  CIteratesT $ do
  x <- get
  case x of
    Nothing -> return Nothing
    Just (CIterator getNext) -> r =<< lift getNext
  where
    r Nothing = do
      put Nothing
      return Nothing
    r (Just (val, iter)) = do
      put $ Just iter
      return $ Just val

processRest :: Monad m => (Iterator a m -> m b) -> IteratesT a m (m b)
processRest func =
  CIteratesT $ do
  rest <- get
  put Nothing
  return . func $ fromMaybe empty rest

