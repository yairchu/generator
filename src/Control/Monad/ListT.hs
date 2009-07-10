module Control.Monad.ListT (
  ListItem(..), ListT(..), cons
) where

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap)
import Control.Monad.Trans (MonadTrans(..))
import Data.Monoid (Monoid(..))

data ListItem l a =
  Nil |
  Cons { headT :: a, tailT :: l a }

data ListT m a = ListT { runListT :: m (ListItem (ListT m) a) }

-- for mappend, fmap, bind
foldrL :: Monad m =>
  (a -> ListT m b -> ListT m b) -> ListT m b -> ListT m a -> ListT m b
foldrL consFunc nilFunc list =
  ListT $ do
    item <- runListT list
    runListT $ case item of
      Nil -> nilFunc
      Cons x xs -> 
        consFunc x $ foldrL consFunc nilFunc xs

instance Monad m => Monoid (ListT m a) where
  mempty = ListT $ return Nil
  mappend = flip (foldrL cons)

instance Monad m => Functor (ListT m) where
  fmap func = foldrL (cons . func) mempty

instance Monad m => Applicative (ListT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (ListT m) where
  return = ListT . return . (`Cons` mempty)
  a >>= b = foldrL mappend mempty $ fmap b a

instance Monad m => MonadPlus (ListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans ListT where
  lift = ListT . (>> return Nil)

cons :: MonadPlus m => a -> m a -> m a
cons = mplus . return

