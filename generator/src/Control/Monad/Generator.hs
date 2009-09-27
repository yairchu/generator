-- | A monad transformer for the creation of Lists.
-- Similar to Python's generators.
--
-- > import Control.Monad.DList (toListT)
-- > import Control.Monad.Identity (Identity(..))
-- > import Data.List.Class (toList)
-- >
-- > hanoi 0 _ _ _ = mempty
-- > hanoi n from to other =
-- >   generate $ do
-- >     yields $ hanoi (n-1) from other to
-- >     yield (from, to)
-- >     yields $ hanoi (n-1) other to from
-- >
-- > > runIdentity . toList . toListT $ hanoi 3 'A' 'B' 'C' :: [(Char, Char)]
-- > [('A','B'),('A','C'),('B','C'),('A','B'),('C','A'),('C','B'),('A','B')]
--

module Control.Monad.Generator (
  GeneratorT, generate, yield, yields
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Monad.Cont (Cont (..))
import Control.Monad.DList (DListT, joinDListT)
import Control.Monad.Trans (MonadTrans(..), MonadIO(..))
import Data.List.Class (cons)
import Data.Monoid (Monoid(..))

-- | A monad transformer to create 'List's.
-- 'generate' transforms a "GeneratorT v m a" to a "DListT m a".
newtype GeneratorT v m a =
  GeneratorT { runGeneratorT :: Cont (DListT m v) a }

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
  lift = GeneratorT . Cont . (joinDListT .) . flip liftM

instance MonadIO m => MonadIO (GeneratorT v m) where
  liftIO = lift . liftIO

-- | /O(1)/, Transform a GeneratorT to a 'DListT'
generate :: Monad m => GeneratorT v m () -> DListT m v
generate = ($ const mempty) . runCont . runGeneratorT

mkContNil :: (r -> r) -> Cont r ()
mkContNil = Cont . (. ($ ()))

-- | /O(1)/, Output a result value
yield :: Monad m => v -> GeneratorT v m ()
yield = GeneratorT . mkContNil . cons

-- | /O(1)/, Output all the values of a 'DListT'.
yields :: Monad m => DListT m v -> GeneratorT v m ()
yields = GeneratorT . mkContNil . mappend

