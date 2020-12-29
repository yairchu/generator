{-# LANGUAGE DerivingStrategies, StandaloneDeriving, DeriveTraversable, DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances, TypeFamilies #-}

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

import Data.List.Class (List(..), ListItem(..), cons, foldrL)

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Generics.Constraints (makeDerivings)
import GHC.Generics (Generic)

newtype ListT m a = ListT { runListT :: m (ListItem (ListT m) a) }
    deriving stock (Functor, Foldable, Traversable, Generic)

makeDerivings [''Eq, ''Ord, ''Read, ''Show] [''ListT]

-- for <>, fmap, bind
foldrL' :: List l => (a -> l b -> l b) -> l b -> l a -> l b
foldrL' consFunc nilFunc =
    joinL . foldrL step (pure nilFunc)
    where
        step x = pure . consFunc x . joinL

instance Monad m => Semigroup (ListT m a) where
    (<>) = flip (foldrL' cons)

instance Monad m => Monoid (ListT m a) where
    mempty = ListT (pure Nil)

instance Monad m => Applicative (ListT m) where
    pure = ListT . pure . (`Cons` mempty)
    (<*>) = ap

instance Monad m => Alternative (ListT m) where
    empty = mempty
    (<|>) = (<>)

instance Monad m => Monad (ListT m) where
    a >>= b = foldrL' (<>) mempty (fmap b a)

instance Monad m => MonadPlus (ListT m)

instance MonadTrans ListT where
    lift = ListT . fmap (`Cons` mempty)

instance Monad m => List (ListT m) where
    type ItemM (ListT m) = m
    runList = runListT
    buildList = ListT

instance MonadIO m => MonadIO (ListT m) where
    liftIO = lift . liftIO
