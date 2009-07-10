{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}

module Data.List.Class (
  List (..), ListItem (..),
  filter, takeWhile,
  cons, fromList,
  foldrL, foldlL
  ) where

import Control.Monad (MonadPlus(..))
import Control.Monad.Identity (Identity(..))
import Data.Foldable (Foldable, foldl')
import Prelude hiding (filter, takeWhile)

data ListItem l a = Nil | Cons a (l a)

class (MonadPlus m, Monad i) => List m i | m -> i where
  joinL :: i (m b) -> m b
  unCons :: m a -> i (ListItem m a)

instance List [] Identity where
  joinL = runIdentity
  unCons [] = Identity Nil
  unCons (x : xs) = Identity $ Cons x xs

cons :: MonadPlus m => a -> m a -> m a
cons = mplus . return

fromList :: (MonadPlus m, Foldable t) => t a -> m a
fromList = foldl' (flip (mplus . return)) mzero

-- for foldrL and foldlL
fold' :: List m i => (a -> m a -> i b) -> i b -> m a -> i b
fold' step nilFunc list = do
  item <- unCons list
  case item of
    Nil -> nilFunc
    Cons x xs -> step x xs

foldrL :: List m i => (a -> i b -> i b) -> i b -> m a -> i b
foldrL consFunc nilFunc = 
  fold' step nilFunc
  where
    step x = consFunc x . foldrL consFunc nilFunc

foldlL :: List m i => (a -> b -> a) -> a -> m b -> i a
foldlL step startVal =
  fold' step' $ return startVal
  where
    step' x = foldlL step $ step startVal x

-- for filter and takeWhile
filter' :: List m i => (i (m a) -> i (m a)) -> (a -> Bool) -> m a -> m a
filter' falseFunc cond =
  joinL . foldrL step (return mzero)
  where
    step x xs
      | cond x = return . cons x $ joinL xs
      | otherwise = falseFunc xs

filter :: List m i => (a -> Bool) -> m a -> m a
filter = filter' id

takeWhile :: List m i => (a -> Bool) -> m a -> m a
takeWhile = filter' . const $ return mzero

