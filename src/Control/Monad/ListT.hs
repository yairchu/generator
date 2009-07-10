module Control.Monad.ListT (
  ListItem(..), ListT(..)
) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Monoid (Monoid(..))

-- should list item replace prelude's ?
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

-- for mappend, fmap
foldrLI :: Monad m =>
  (a -> ListT m b -> ListItem (ListT m) b) ->
  ListT m b -> ListT m a -> ListT m b
foldrLI consFunc =
  foldrL step
  where
    step x = ListT . return . consFunc x

instance Monad m => Monoid (ListT m a) where
  mempty = ListT $ return Nil
  mappend = flip (foldrLI Cons)

instance Monad m => Functor (ListT m) where
  fmap func = foldrLI (Cons . func) mempty

instance Monad m => Monad (ListT m) where
  return x = ListT . return $ Cons x mempty
  a >>= b = foldrL mappend mempty $ fmap b a

instance Monad m => MonadPlus (ListT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans ListT where
  lift = ListT . (>> return Nil)

