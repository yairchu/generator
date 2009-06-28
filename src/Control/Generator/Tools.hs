{-# OPTIONS -O2 -Wall #-}

module Control.Generator.Tools(
  append, execute, fromList, iconcat,
  ifoldl, ifoldl', ifoldr, ifoldr', ilength, imap,
  ifilter, itake, iTakeWhile, izip, liftProdMonad, toList
  ) where

import Control.Generator (
  Producer, cons, empty, evalConsumerT,
  mmerge, next, processRest)
import Control.Generator.ProducerT (produce, yield)
import Control.Monad (forever, liftM)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.Trans (MonadTrans(..))

-- naming: for everything that's in prelude I add an "i" prefix,
-- for convenient importing

ifoldl :: Monad m => (a -> b -> m a) -> a -> Producer b m -> m a
ifoldl func startVal =
  evalConsumerT $ r startVal =<< next
  where
    r s Nothing = return s
    r s (Just v) = do
      x <- lift (func s v)
      r x =<< next

ifoldl' :: Monad m => (a -> b -> m a) -> a -> Producer b m -> m a
ifoldl' step =
  ifoldl step'
  where
    step' a b = do
      x <- step a b
      x `seq` return x

-- consFunc takes "m b" and not "b" so could avoid running the rest
ifoldr :: Monad m => (a -> m b -> m b) -> m b -> Producer a m -> m b
ifoldr consFunc nilFunc =
  evalConsumerT $ r =<< next
  where
    r Nothing = lift nilFunc
    r (Just v) =
      lift . consFunc v =<< processRest (r =<< next)

-- for operations that build Producers, combine step with the mmerge etc boiler-plate
ifoldr' :: Monad m => (b -> Producer a m -> Producer a m) -> Producer a m -> Producer b m -> Producer a m
ifoldr' consFunc start =
  mmerge . ifoldr step (return start)
  where
    step x = return . consFunc x . mmerge

imap :: Monad m => (a -> m b) -> Producer a m -> Producer b m
imap func =
  ifoldr' step empty
  where
    step a bs =
      mmerge $ do
      b <- func a
      return $ cons b bs

execute :: Monad m => Producer a m -> m ()
execute = ifoldl' (const . return) ()

ifilter :: Monad m => (a -> m Bool) -> Producer a m -> Producer a m
ifilter cond =
  ifoldr' r empty
  where
    r x xs =
      mmerge $ do
      b <- cond x
      return $ if b then cons x xs else xs

-- TODO: uses ifoldl because I think with ifoldr it would use much mem, right?
toList :: (Monad m) => Producer a m -> m [a]
toList =
  liftM reverse . ifoldl step []
  where
    step xs x = return $ x : xs

iTakeWhile :: Monad m => (a -> Bool) -> Producer a m -> Producer a m
iTakeWhile func =
  ifoldr' r empty
  where
    r x xs = if func x then cons x xs else empty

fromList :: (Monad m) => [a] -> Producer a m
fromList = foldr cons empty

append :: Monad m => Producer a m -> Producer a m -> Producer a m
append a b = ifoldr' cons b a

iconcat :: Monad m => Producer (Producer a m) m -> Producer a m
iconcat = ifoldr' append empty

ilength :: (Monad m, Integral i) => Producer a m -> m i
ilength = ifoldl' (const . return . (+ 1))  0

itake :: (Monad m, Integral i) => i -> Producer a m -> Producer a m
itake count =
  mmerge . evalConsumerT (r0 count)
  where
    r0 0 = return empty
    r0 c = r1 c =<< next
    r1 _ Nothing = return empty
    r1 c (Just v) =
      return . cons v . mmerge =<< processRest (r0 (c-1))

liftProdMonad ::
  (Monad (t m), Monad m, MonadTrans t) =>
  Producer a m -> Producer a (t m)
liftProdMonad =
  mmerge . lift . evalConsumerT (r =<< next)
  where
    r Nothing = return empty
    r (Just v) =
      return . cons v . mmerge . lift =<< processRest (r =<< next)

izip :: Monad m => Producer a m -> Producer b m -> Producer (a, b) m
izip prodA prodB =
  produce .
  flip evalConsumerT (liftProdMonad prodA) .
  flip evalConsumerT (liftProdMonad (liftProdMonad prodB)) $ do
  runMaybeT . forever $ do
    a <- MaybeT $ lift next
    b <- MaybeT next
    lift . lift . lift $ yield (a, b)
    return ()
  return ()

