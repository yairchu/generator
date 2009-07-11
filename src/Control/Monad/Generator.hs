{-# OPTIONS -O2 -Wall #-}

-- | A monad transformer for the creation of 'DListT's.
-- Similar to Python's generators.

module Control.Monad.Generator (
  GeneratorT, produce, yield, yields
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Cont (Cont (..))
import Control.Monad.DList (DListT)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.List.Class (cons, joinL)
import Data.Monoid (Monoid(..))

newtype GeneratorT v m a =
  GeneratorT { runGeneratorT :: Cont (DListT m v) a }

instance Monad m => Functor (GeneratorT v m) where
  fmap = liftM
instance Monad m => Applicative (GeneratorT v m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (GeneratorT v m) where
  return = GeneratorT . return
  GeneratorT a >>= f = GeneratorT $ a >>= runGeneratorT . f
  fail = lift . fail

instance MonadTrans (GeneratorT v) where
  lift m = GeneratorT . Cont $ joinL . (`liftM` m)

instance MonadIO m => MonadIO (GeneratorT v m) where
  liftIO = lift . liftIO

-- | /O(1)/, Transform a GeneratorT to a 'DListT'
produce :: Monad m => GeneratorT v m () -> DListT m v
produce = ($ const mempty) . runCont . runGeneratorT

-- | /O(1)/, Output a result value
yield :: Monad m => v -> GeneratorT v m ()
yield x = GeneratorT . Cont $ cons x . ($ ())

-- | /O(1)/, Output all the values of another 'DListT'.
yields :: Monad m => DListT m v -> GeneratorT v m ()
yields xs = GeneratorT . Cont $ mappend xs . ($ ())

