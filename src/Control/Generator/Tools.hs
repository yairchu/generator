{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE Rank2Types #-}

module Control.Generator.Tools(
  consumeLift, execute, fromList, concatP,
  foldlP, foldlP', foldrP, foldrP', lastP, lengthP, mapP,
  filterP, scanlP, takeP, takeWhileP, zipP, zipWithP,
  liftProdMonad, toList, transformProdMonad
  ) where

import Control.Generator.Consumer (
  ConsumerT, evalConsumerT, next, consumeRestM)
import Control.Generator.Producer (
  append, cons, empty)
import Control.Generator.Producer (Producer, joinP)
import Control.Generator.ProducerT (produce, yield)
import Control.Monad (forever, liftM, liftM2)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (StateT(..), evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Function (fix)

-- naming: for everything that's in prelude I add an "i" prefix,
-- for convenient importing

foldlP :: Monad m => (a -> b -> m a) -> a -> Producer m b -> m a
foldlP func startVal =
  evalConsumerT $ r startVal =<< next
  where
    r s Nothing = return s
    r s (Just v) = do
      x <- lift (func s v)
      r x =<< next

foldlP' :: Monad m => (a -> b -> m a) -> a -> Producer m b -> m a
foldlP' step =
  foldlP step'
  where
    step' a b = do
      x <- step a b
      x `seq` return x

-- consFunc takes "m b" and not "b" so could avoid running the rest
foldrP :: Monad m => (a -> m b -> m b) -> m b -> Producer m a -> m b
foldrP consFunc nilFunc =
  evalConsumerT . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> lift nilFunc
      Just x -> lift . consFunc x =<< consumeRestM rest

-- for operations that build Producers, combine step with the joinP etc boiler-plate
foldrP' :: Monad m => (b -> Producer m a -> Producer m a) -> Producer m a -> Producer m b -> Producer m a
foldrP' consFunc start =
  joinP . foldrP step (return start)
  where
    step x = return . consFunc x . joinP

singleItemM :: Monad m => m a -> Producer m a
singleItemM = joinP . liftM (`cons` empty)

consM :: Monad m => m a -> Producer m a -> Producer m a
consM = append . singleItemM

mapP :: Monad m => (a -> m b) -> Producer m a -> Producer m b
mapP func = foldrP' (consM . func) empty

execute :: Monad m => Producer m a -> m ()
execute = foldlP' (const . return) ()

toList :: (Monad m) => Producer m a -> m [a]
toList =
  foldrP step $ return []
  where
    step x = (return . (x :) =<<)

-- used in filterP and takeWhileP
filterStepP :: Monad m => (a -> m Bool) -> (Producer m a -> Producer m a) -> a -> Producer m a -> Producer m a
filterStepP cond onFalse x xs =
  joinP $ do
    b <- cond x
    return $ if b then cons x xs else onFalse xs

filterP :: Monad m => (a -> m Bool) -> Producer m a -> Producer m a
filterP cond =
  foldrP' (filterStepP cond id) empty

takeWhileP :: Monad m => (a -> m Bool) -> Producer m a -> Producer m a
takeWhileP cond =
  foldrP' (filterStepP cond (const empty)) empty

fromList :: (Monad m) => [a] -> Producer m a
fromList = foldr cons empty

concatP :: Monad m => Producer m (Producer m a) -> Producer m a
concatP = foldrP' append empty

lengthP :: (Monad m, Integral i) => Producer m a -> m i
lengthP = foldlP' (const . return . (+ 1))  0

takeP :: (Monad m, Integral i) => i -> Producer m a -> Producer m a
takeP count =
  joinP . evalConsumerT (foldr r (return empty) [1..count])
  where
    r _ rest = do
      mx <- next
      case mx of
        Nothing -> return empty
        Just x ->
          liftM (cons x . joinP) $ consumeRestM rest

maybeForever :: Monad m => MaybeT m a -> m ()
maybeForever = (>> return ()) . runMaybeT . forever

lastP :: Monad m => Producer m a -> m a
lastP =
  evalConsumerT .
  liftM snd .
  (`runStateT` undefined) .
  maybeForever $ MaybeT (lift next) >>= put

transformProdMonad :: (Monad m, Monad s) =>
  (forall a. m a -> m (s a)) -> Producer m v -> m (Producer s v)
transformProdMonad trans =
  evalConsumerT . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> return empty
      Just x ->
        liftM (cons x . joinP) $
        lift . trans =<< consumeRestM rest

liftProdMonad ::
  (Monad (t m), Monad m, MonadTrans t) =>
  Producer m a -> Producer (t m) a
liftProdMonad = joinP . lift . transformProdMonad (return . lift)

consumeLift ::
  (Monad (t m), Monad m, MonadTrans t) =>
  ConsumerT a (t m) b -> Producer m a -> t m b
consumeLift consumer = evalConsumerT consumer . liftProdMonad

zipP :: Monad m => Producer m a -> Producer m b -> Producer m (a, b)
zipP p1 p2 =
  produce . (`consumeLift` p1) . (`consumeLift` liftProdMonad p2) .
  maybeForever $
  liftM2 (,) (MaybeT (lift next)) (MaybeT next) >>=
  lift . lift . lift . yield

zipWithP :: Monad m => (a -> b -> m c) -> Producer m a -> Producer m b -> Producer m c
zipWithP f p = mapP (uncurry f) . zipP p

scanlP :: Monad m => (a -> b -> m a) -> a -> Producer m b -> Producer m a
scanlP func start =
  produce . consumeLift (evalStateT r start)
  where
    r =
      maybeForever $ do
      s <- get
      lift . lift . lift $ yield s
      x <- MaybeT $ lift next
      put =<< (lift . lift . lift . lift) (func s x)

