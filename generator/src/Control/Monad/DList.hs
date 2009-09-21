{-# LANGUAGE TypeFamilies #-}

-- | A difference-list monad transformer / a monadic difference-list.
--
-- Difference lists are lists with /O(1)/ append (instead of /O(N)/).
--
-- Transforming a difference list to a list is /O(1)/,
-- a must be done to access a difference list.
-- The transformation from a list to a difference list is /O(N)/.

module Control.Monad.DList (
  DListT (..), toListT, joinDListT
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), liftM, ap)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans (MonadTrans(..))
import Data.List.Class (List(..), cons)
import Data.Monoid (Monoid(..))

-- | A monadic difference-list
newtype DListT m a = DListT { runDListT :: ListT m a -> ListT m a }

toListT :: Monad m => DListT m a -> ListT m a
toListT = (`runDListT` mzero)

joinDListT :: Monad m => m (DListT m a) -> DListT m a
joinDListT action =
  DListT (joinL . (`liftM` action) . flip runDListT)

instance Monoid (DListT l a) where
  mempty = DListT id
  mappend (DListT a) (DListT b) = DListT $ a . b

instance Monad m => Functor (DListT m) where
  fmap func = DListT . mplus . liftM func . toListT

instance Monad m => Monad (DListT m) where
  return = DListT . cons
  a >>= b = DListT . mplus $ toListT a >>= liftM toListT b

instance Monad m => Applicative (DListT m) where
  pure = return
  (<*>) = ap

instance Monad m => MonadPlus (DListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans DListT where
  lift = DListT . mappend . lift

