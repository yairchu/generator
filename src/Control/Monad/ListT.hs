{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- Module is called ListT because List is taken by mtl

-- | A list monad transformer / a monadic list.
--
-- Monadic list example:
--   A program which reads numbers from the user and accumulates them.
--
-- > import Control.Monad.ListT (ListT)
-- > import Data.List.Class (joinL, repeat, scanl, sequence, sequence_, takeWhile)
-- > import Prelude hiding (repeat, scanl, sequence, sequence_, takeWhile)
-- >
-- > main =
-- >   sequence_ .
-- >   fmap print .
-- >   scanl (+) 0 .
-- >   fmap (fst . head) .
-- >   takeWhile (not . null) .
-- >   fmap reads .
-- >   joinL . sequence $
-- >   (repeat getLine :: ListT IO (IO String))

module Control.Monad.ListT (
  ListItem(..), ListT(..), foldrListT
) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap, liftM)
-- import Control.Monad.Cont.Class (MonadCont(..))
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.Monoid (Monoid(..))

data ListItem l a =
  Nil |
  Cons { headL :: a, tailL :: l a }

data ListT m a = ListT { runListT :: m (ListItem (ListT m) a) }

-- | foldr for ListT
foldrListT :: Monad m => (a -> m b -> m b) -> m b -> ListT m a -> m b
foldrListT consFunc nilFunc list = do
  item <- runListT list
  case item of
    Nil -> nilFunc
    Cons x xs -> consFunc x $ foldrListT consFunc nilFunc xs

-- for mappend, fmap, bind
foldrListT' :: Monad m =>
  (a -> ListT m b -> ListT m b) -> ListT m b -> ListT m a -> ListT m b
foldrListT' consFunc nilFunc =
  ListT . foldrListT step (runListT nilFunc)
  where
    step x = runListT . consFunc x . ListT

-- like generic cons except using that one
-- would cause an infinite loop
cons :: Monad m => a -> ListT m a -> ListT m a
cons x = ListT . return . Cons x

instance Monad m => Monoid (ListT m a) where
  mempty = ListT $ return Nil
  mappend = flip (foldrListT' cons)

instance Monad m => Functor (ListT m) where
  fmap func = foldrListT' (cons . func) mempty

instance Monad m => Monad (ListT m) where
  return = ListT . return . (`Cons` mempty)
  a >>= b = foldrListT' mappend mempty $ fmap b a

instance Monad m => Applicative (ListT m) where
  pure = return
  (<*>) = ap

instance Monad m => MonadPlus (ListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans ListT where
  lift = ListT . liftM (`Cons` mempty)

-- YUCK:
-- I can't believe I'm doing this,
-- for compatability with mtl's ListT.
-- I hate the O(N^2) code length auto-lifts. DRY!!

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO

{-
-- TODO: understand and verify this instance :)
instance MonadCont m => MonadCont (ListT m) where
  callCC f =
    ListT $ callCC thing
    where
      thing c = runListT . f $ ListT . c . (`Cons` mempty)
-}

instance MonadError e m => MonadError e (ListT m) where
  throwError = lift . throwError
  catchError m = ListT . catchError (runListT m) . (runListT .)

instance MonadReader s m => MonadReader s (ListT m) where
  ask = lift ask
  local f = ListT . local f . runListT

instance MonadState s m => MonadState s (ListT m) where
  get = lift get
  put = lift . put

