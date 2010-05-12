{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- Module is called ListT because List is taken by mtl

-- | A list monad transformer / a monadic list.
--
-- Monadic list example:
--   A program which reads numbers from the user and accumulates them.
--
-- > import Control.Monad (join)
-- > import Control.Monad.ListT (ListT)
-- > import Control.Monad.Trans (lift)
-- > import Data.List.Class (execute, repeat, scanl, takeWhile, mapL)
-- > import Prelude hiding (repeat, scanl, takeWhile)
-- > 
-- > main =
-- >     execute . mapL print .
-- >     scanl (+) 0 .
-- >     fmap (fst . head) .
-- >     takeWhile (not . null) .
-- >     fmap reads $ do
-- >       repeat ()
-- >       lift getLine :: ListT IO String

module Control.Monad.ListT (ListT(..)) where

import Data.List.Class (List(..), ListItem(..), foldrL)

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap, liftM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Monoid (Monoid(..))

newtype ListT m a =
  ListT { runListT :: m (ListItem (ListT m) a) }

deriving instance (Eq (m (ListItem (ListT m) a))) => Eq (ListT m a)
deriving instance (Ord (m (ListItem (ListT m) a))) => Ord (ListT m a)
deriving instance (Read (m (ListItem (ListT m) a))) => Read (ListT m a)
deriving instance (Show (m (ListItem (ListT m) a))) => Show (ListT m a)

-- for mappend, fmap, bind
foldrL' :: List l => (a -> l b -> l b) -> l b -> l a -> l b
foldrL' consFunc nilFunc =
  joinL . foldrL step (return nilFunc)
  where
    step x = return . consFunc x . joinL

-- like generic cons except using that one
-- would cause an infinite loop
cons :: Monad m => a -> ListT m a -> ListT m a
cons x = ListT . return . Cons x

instance Monad m => Monoid (ListT m a) where
  mempty = ListT $ return Nil
  mappend = flip (foldrL' cons)

instance Monad m => Functor (ListT m) where
  fmap func = foldrL' (cons . func) mempty

instance Monad m => Monad (ListT m) where
  return = ListT . return . (`Cons` mempty)
  a >>= b = foldrL' mappend mempty (fmap b a)

instance Monad m => Applicative (ListT m) where
  pure = return
  (<*>) = ap

instance Monad m => MonadPlus (ListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans ListT where
  lift = ListT . liftM (`Cons` mempty)

instance Monad m => List (ListT m) where
  type ItemM (ListT m) = m
  runList = runListT
  joinL = ListT . (>>= runList)

instance MonadIO m => MonadIO (ListT m) where
  liftIO = lift . liftIO
