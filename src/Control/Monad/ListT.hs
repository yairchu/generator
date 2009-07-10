{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- Module is called ListT because List is taken by mtl

module Control.Monad.ListT (
  -- | The ListT monad transformer
  ListItem(..), ListT(..),
  -- | Compatability with mtl's ListT
  runListT, mapListT
) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap, liftM)
-- import Control.Monad.Cont.Class (MonadCont(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.List.Class (
  List(..), ListItem(..), cons, foldrL, fromList, toList)
import Data.Monoid (Monoid(..))

-- runListT' called this way because am implementing mtl's runListT
data ListT m a = ListT { runListT' :: m (ListItem (ListT m) a) }

-- for mappend, fmap, bind
foldrL' :: Monad m =>
  (a -> ListT m b -> ListT m b) -> ListT m b -> ListT m a -> ListT m b
foldrL' consFunc nilFunc =
  ListT . foldrL step (runListT' nilFunc)
  where
    step x = runListT' . consFunc x . ListT

instance Monad m => Monoid (ListT m a) where
  mempty = ListT $ return Nil
  mappend = flip (foldrL' cons)

instance Monad m => Functor (ListT m) where
  fmap func = foldrL' (cons . func) mempty

instance Monad m => Applicative (ListT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return = ListT . return . (`Cons` mempty)
  a >>= b = foldrL' mappend mempty $ fmap b a

instance Monad m => MonadPlus (ListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans ListT where
  lift = ListT . (>> return Nil)

instance Monad m => List (ListT m) m where
  joinL = ListT . (>>= runListT')
  unCons = runListT'

-- | Acronym for toList for compatability with mtl's ListT
runListT :: Monad m => ListT m a -> m [a]
runListT = toList

-- | An extremely odd function implemented in mtl's ListT
mapListT :: (Monad m, Monad n) => (m [a] -> n [b]) -> ListT m a -> ListT n b
mapListT f = joinL . liftM fromList . f . runListT

-- YUCK:
-- I can't believe I'm doing this,
-- for compatability with mtl's ListT.
-- I hate the O(N^2) code length auto-lifts. DRY!!

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO

-- ((a -> m b) -> m a) -> m a
-- ((a -> (b -> r) -> r) -> (a -> r) -> r) -> (a -> r) -> r

{-
-- TODO: understand and verify this instance :)
instance MonadCont m => MonadCont (ListT m) where
  callCC f =
    ListT $ callCC thing
    where
      thing c = runListT' . f $ ListT . c . (`Cons` mempty)
-}

instance MonadError e m => MonadError e (ListT m) where
  throwError = lift . throwError
  catchError m = ListT . catchError (runListT' m) . (runListT' .)

instance MonadReader s m => MonadReader s (ListT m) where
  ask = lift ask
  local f = ListT . local f . runListT'

instance MonadState s m => MonadState s (ListT m) where
  get = lift get
  put = lift . put

