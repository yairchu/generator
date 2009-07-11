{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Data.DList.Generic (
  DList (..), toList
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), liftM, ap)
import Data.List.Class (List(..), cons)
import Data.Monoid (Monoid(..))

data DList l a = DList { runDList :: l a -> l a }

-- This requires MonadPlus l,
-- but actually it only needs Monoid (l a).
-- The reason MonadPlus is used is that all users need MonadPlus,
-- and that MonadPlus doesn't automatically mean Monoid :(
toList :: MonadPlus l => DList l a -> l a
toList = (`runDList` mzero)

instance Monoid (DList l a) where
  mempty = DList id
  mappend (DList a) (DList b) = DList $ a . b

instance MonadPlus l => Functor (DList l) where
  fmap func = DList . mplus . liftM func . toList

instance MonadPlus l => Monad (DList l) where
  return = DList . cons
  a >>= b = DList . mplus $ toList a >>= liftM toList b

instance MonadPlus l => Applicative (DList l) where
  pure = return
  (<*>) = ap

instance MonadPlus l => MonadPlus (DList l) where
  mzero = mempty
  mplus = mappend

instance List (t m) m => List (DList (t m)) m where
  joinL = DList . mplus . joinL . liftM toList
  foldrL consFunc nilFunc = foldrL consFunc nilFunc . (`runDList` mzero)
