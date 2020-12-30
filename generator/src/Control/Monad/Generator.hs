{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveFunctor, DerivingStrategies, GeneralisedNewtypeDeriving #-}

-- | A monad transformer for the creation of Lists.
-- Similar to Python's generators.
--
-- > import Control.Monad.Identity (Identity(..))
-- > import Data.List.Class (toList)
-- >
-- > hanoi 0 _ _ _ = return ()
-- > hanoi n from to other = do
-- >     hanoi (n-1) from other to
-- >     yield (from, to)
-- >     hanoi (n-1) other to from
-- >
-- > > runIdentity . toList . generate $ hanoi 3 'A' 'B' 'C' :: [(Char, Char)]
-- > [('A','B'),('A','C'),('B','C'),('A','B'),('C','A'),('C','B'),('A','B')]
--

module Control.Monad.Generator (
    GeneratorT(..), generate, yield, breakGenerator
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.ListT (ListT)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Cont (ContT(..), mapContT)
import Data.List.Class (cons)

-- | A monad transformer to create 'List's.
-- 'generate' transforms a "GeneratorT v m a" to a "ListT m a".
newtype GeneratorT v m a =
    GeneratorT { runGeneratorT :: ContT v (ListT m) a }
    deriving stock Functor
    deriving newtype (Applicative, Monad, MonadIO)

instance Monad m => Semigroup (GeneratorT v m a) where
    GeneratorT (ContT a) <> GeneratorT (ContT b) = (GeneratorT . ContT) (a <> b)

instance Monad m => Monoid (GeneratorT v m a) where
    mempty = (GeneratorT . ContT) mempty

instance Monad m => Alternative (GeneratorT v m) where
    empty = mempty
    (<|>) = (<>)

instance MonadFail m => MonadFail (GeneratorT v m) where
    fail = lift . fail

instance MonadTrans (GeneratorT v) where
    lift = GeneratorT . lift . lift

generate :: Monad m => GeneratorT v m () -> ListT m v
generate = (`runContT` mempty) . runGeneratorT

modifyRes :: (ListT m a -> ListT m a) -> GeneratorT a m ()
modifyRes = GeneratorT . (`mapContT` pure ())

yield :: Monad m => v -> GeneratorT v m ()
yield = modifyRes . cons

breakGenerator :: Monad m => GeneratorT v m a
breakGenerator = GeneratorT . ContT $ mempty
