{-# LANGUAGE ExistentialQuantification, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}

-- Module is called StreamT because Control.Monad.Stream
-- is occupied by stream-fusion
-- for its versions of Control.Monad's list operations.
--
-- This module is heavily modelled after the paper:
-- "Stream Fusion: From Lists to Streams to Nothing at All"
-- by Duncan Coutts, Roman Leshchinskiy and Don Stewart (IFCP'07)
--
-- This differs from the "stream-fusion" package by:
-- * Offering "Stream Fusion" for monadic lists.
-- * Uses type classes (List, Monad, Functor, etc.)
--   instead of using rewrite rules and alternative reimplementations
--   of common list functions. So the user knows he uses Streams,
--   but their use is seamless in the intermediate functions that
--   use any instance of any type-class that both lists and streams implement.

module Control.Monad.StreamT
  ( StreamT(..), Step(..)
  , enumFromTo
  ) where

import Data.List.Class (List(..), ListItem(..))

import Control.Applicative (Applicative(..))
import Control.Monad (MonadPlus(..), ap, liftM)
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Monoid (Monoid(..))
import Prelude hiding (enumFromTo)

data Step a s = Done | Yield a s | Skip s
data StreamT m a = forall s. StreamT
  { streamStep :: s -> m (Step a s)
  , streamPos :: s
  }

-- todo: either of
-- * get rid of Monoid for performance
-- * make rewrite rules to make its performance sane
-- * conclude that none of the above is needed
instance Monad m => Monoid (StreamT m a) where
  mempty = StreamT ((return . return) Done) ()
  mappend (StreamT aStep aStart) (StreamT bStep bStart) =
    StreamT step (Left aStart)
    where
      step (Left pos) = do
        next <- aStep pos
        return $ case next of
          Done -> Skip (Right bStart)
          Yield val newPos -> Yield val (Left newPos)
          Skip newPos -> Skip (Left newPos)
      step (Right pos) = do
        next <- bStep pos
        return $ case next of
          Done -> Done
          Yield val newPos -> Yield val (Right newPos)
          Skip newPos -> Skip (Right newPos)

instance Monad m => Functor (StreamT m) where
  fmap func (StreamT step pos) =
    StreamT (liftM f . step) pos
    where
      f (Yield x s) = Yield (func x) s
      f Done = Done
      f (Skip s) = Skip s

instance Monad m => Monad (StreamT m) where
  return =
    StreamT step . Just
    where
      step (Just x) = return $ Yield x Nothing
      step Nothing = return Done
  (StreamT aStep aStart) >>= b =
    StreamT step (aStart, Nothing)
    where
      step (aPos, Nothing) = do
        next <- aStep aPos
        return $ case next of
          Done -> Done
          Yield val newPos -> Skip (newPos, Just (b val))
          Skip newPos -> Skip (newPos, Nothing)
      step (aPos, Just (StreamT bStep bPos)) = do
        next <- bStep bPos
        return $ case next of
          Done -> Skip (aPos, Nothing)
          Yield val newPos -> Yield val (aPos, Just (StreamT bStep newPos))
          Skip newPos -> Skip (aPos, Just (StreamT bStep newPos))

instance Monad m => Applicative (StreamT m) where
  pure = return
  (<*>) = ap

-- see todo of Monoid instance
instance Monad m => MonadPlus (StreamT m) where
  mzero = mempty
  mplus = mappend

instance MonadTrans StreamT where
  lift =
    StreamT step . Just
    where
      step (Just action) = do
        val <- action
        return $ Yield val Nothing
      step Nothing = return Done

instance Monad m => List (StreamT m) where
  type ItemM (StreamT m) = m
  runList (StreamT step pos) = do
    next <- step pos
    case next of
      Done -> return Nil
      Yield val newPos -> return . Cons val $ StreamT step newPos
      Skip newPos -> runList $ StreamT step newPos
  joinL =
    StreamT step . Left
    where
      step (Left action) = liftM (Skip . Right) action
      step (Right (StreamT sStep pos)) = do
        next <- sStep pos
        return $ case next of
          Done -> Done
          Yield val newPos -> Yield val . Right . StreamT sStep $ newPos
          Skip newPos -> Skip . Right . StreamT sStep $ newPos

enumFromTo :: (Enum a, Eq a, Monad m) => a -> a -> StreamT m a
enumFromTo start end =
  StreamT step (Just start)
  where
    step (Just x)
      | x == end = return . Yield x $ Nothing
      | otherwise = return . Yield x . Just . succ $ x
    step Nothing = return Done

instance MonadState s m => MonadState s (StreamT m) where
  get = lift get
  put = lift . put

