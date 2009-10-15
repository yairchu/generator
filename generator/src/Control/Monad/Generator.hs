{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

-- | A monad transformer for the creation of Lists.
-- Similar to Python's generators.
--
-- > import Control.Monad.Identity (Identity(..))
-- > import Data.List.Class (toList)
-- >
-- > hanoi 0 _ _ _ = return ()
-- > hanoi n from to other = do
-- >   hanoi (n-1) from other to
-- >   yield (from, to)
-- >   hanoi (n-1) other to from
-- >
-- > > runIdentity . toList . generate $ hanoi 3 'A' 'B' 'C' :: [(Char, Char)]
-- > [('A','B'),('A','C'),('B','C'),('A','B'),('C','A'),('C','B'),('A','B')]
--

module Control.Monad.Generator (
  GeneratorT(..), generate, yield, breakGenerator
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Cont (ContT (..), mapContT)
import Control.Monad.ListT (ListT)
import Control.Monad.Reader.Class (MonadReader(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.List.Class (cons)
import Data.Monoid (Monoid(..))

-- | A monad transformer to create 'List's.
-- 'generate' transforms a "GeneratorT v m a" to a "ListT m a".
newtype GeneratorT v m a =
  GeneratorT { runGeneratorT :: ContT v (ListT m) a }

instance Monad m => Functor (GeneratorT v m) where
  fmap = liftM

instance Monad m => Monad (GeneratorT v m) where
  return = GeneratorT . return
  GeneratorT a >>= f = GeneratorT $ a >>= runGeneratorT . f
  fail = lift . fail

instance Monad m => Applicative (GeneratorT v m) where
  pure = return
  (<*>) = ap

instance MonadTrans (GeneratorT v) where
  lift = GeneratorT . lift . lift

generate :: Monad m => GeneratorT v m () -> ListT m v
generate = (`runContT` const mempty) . runGeneratorT

modifyRes :: Monad m => (ListT m a -> ListT m a) -> GeneratorT a m ()
modifyRes = GeneratorT . (`mapContT` return ())

yield :: Monad m => v -> GeneratorT v m ()
yield = modifyRes . cons

breakGenerator :: Monad m => GeneratorT v m a
breakGenerator = GeneratorT . ContT . const $ mempty

-- Yuck: (code duplication)

instance MonadIO m => MonadIO (GeneratorT v m) where
  liftIO = lift . liftIO

inGeneratorT
  :: (ContT v0 (ListT m0) a0 -> ContT v1 (ListT m1) a1)
  -> GeneratorT v0 m0 a0 -> GeneratorT v1 m1 a1
inGeneratorT f = GeneratorT . f . runGeneratorT

instance MonadReader s m => MonadReader s (GeneratorT v m) where
  ask = lift ask
  local = inGeneratorT . mapContT . local

instance MonadState s m => MonadState s (GeneratorT v m) where
  get = lift get
  put = lift . put

