{-# LANGUAGE CPP, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | A list monad transformer / a monadic list.
--
-- Monadic list example:
--   A program which reads numbers from the user and accumulates them.
--
-- > import Control.Monad.ListT.Funcs (repeatM)
-- > import Data.List.Class (execute, scanl, takeWhile, mapL)
-- > import Prelude hiding (scanl, takeWhile)
-- >
-- > main =
-- >     execute . mapL print .
-- >     scanl (+) 0 .
-- >     fmap (fst . head) .
-- >     takeWhile (not . null) .
-- >     fmap reads $ repeatM getLine
--
-- Note:
-- The `transformers` package also has a `ListT` type,
-- which oddly enough it is not a list monad transformer.
-- This module was deliberately named differently from `transformers`'s module.

module Control.Monad.ListT (ListT(..)) where

import Data.List.Class (List(..), ListItem(..), foldrL)

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))

newtype ListT m a = ListT { runListT :: m (ListItem (ListT m) a) }

deriving instance (Eq (m (ListItem (ListT m) a))) => Eq (ListT m a)
deriving instance (Ord (m (ListItem (ListT m) a))) => Ord (ListT m a)
deriving instance (Read (m (ListItem (ListT m) a))) => Read (ListT m a)
deriving instance (Show (m (ListItem (ListT m) a))) => Show (ListT m a)

-- for mappend, fmap, bind
foldrL' :: List l => (a -> l b -> l b) -> l b -> l a -> l b
foldrL' consFunc nilFunc =
    joinL . foldrL step (pure nilFunc)
    where
        step x = pure . consFunc x . joinL

instance Monad m => Semigroup (ListT m a) where
    (<>) = flip (foldrL' cons)

instance Monad m => Monoid (ListT m a) where
    mempty = ListT (pure Nil)

instance Functor m => Functor (ListT m) where
    fmap func (ListT action) =
        ListT (fmap f action)
        where
            f Nil = Nil
            f (Cons x xs) = Cons (func x) (fmap func xs)

instance Monad m => Monad (ListT m) where
    return = ListT . pure . (`Cons` mempty)
    a >>= b = foldrL' mappend mempty (fmap b a)

instance Monad m => Applicative (ListT m) where
    pure = return
    (<*>) = ap

instance Monad m => Alternative (ListT m) where
    empty = mempty
    (<|>) = mappend

instance Monad m => MonadPlus (ListT m) where
    mzero = mempty
    mplus = mappend

instance MonadTrans ListT where
    lift = ListT . fmap (`Cons` mempty)

instance Monad m => List (ListT m) where
    type ItemM (ListT m) = m
    runList = runListT
    joinL = ListT . (>>= runList)
    cons x = ListT . pure . Cons x

instance MonadIO m => MonadIO (ListT m) where
    liftIO = lift . liftIO
