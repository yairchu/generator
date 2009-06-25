module Iterator (
  Iterator, IteratesT,
  evalIteratesT, cons, iterator, next, nil, takeRest
  ) where

import Control.Monad.State (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))

type Iterator' v m = m (Maybe (v, Iterator v m))
newtype Iterator v m = CIterator (Iterator' v m)

iterator :: Iterator' v m -> Iterator v m
iterator = CIterator

nil :: Monad m => Iterator' v m
nil = return Nothing

cons :: Monad m => v -> Iterator' v m -> Iterator' v m
cons v = return . Just . ((,) v) . iterator

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

takeRest :: Monad m => IteratesT v m (Iterator v m)
takeRest =
  CIteratesT $ do
  it <- get
  put Nothing
  case it of
    Nothing -> return . iterator $ return Nothing
    Just x -> return x

