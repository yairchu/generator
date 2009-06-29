{-# OPTIONS -O2 -Wall #-}

module Control.Generator.Tools(
  append, execute, fromList, iconcat,
  ifoldl, ifoldl', ifoldr, ifoldr', ilength, imap,
  ifilter, itake, iTakeWhile, izip, izipWith, izipP2,
  liftProdMonad, toList
  ) where

import Control.Generator (
  Producer, cons, empty, ConsumerT, evalConsumerT,
  mmerge, next, processRest)
import Control.Generator.ProducerT (ProducerT, produce, yield)
import Control.Monad (forever, liftM2)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.Trans (MonadTrans(..))

-- naming: for everything that's in prelude I add an "i" prefix,
-- for convenient importing

ifoldl :: Monad m => (a -> b -> m a) -> a -> Producer m b -> m a
ifoldl func startVal =
  evalConsumerT $ r startVal =<< next
  where
    r s Nothing = return s
    r s (Just v) = do
      x <- lift (func s v)
      r x =<< next

ifoldl' :: Monad m => (a -> b -> m a) -> a -> Producer m b -> m a
ifoldl' step =
  ifoldl step'
  where
    step' a b = do
      x <- step a b
      x `seq` return x

-- consFunc takes "m b" and not "b" so could avoid running the rest
ifoldr :: Monad m => (a -> m b -> m b) -> m b -> Producer m a -> m b
ifoldr consFunc nilFunc =
  evalConsumerT $ r =<< next
  where
    r Nothing = lift nilFunc
    r (Just v) =
      lift . consFunc v =<< processRest (r =<< next)

-- for operations that build Producers, combine step with the mmerge etc boiler-plate
ifoldr' :: Monad m => (b -> Producer m a -> Producer m a) -> Producer m a -> Producer m b -> Producer m a
ifoldr' consFunc start =
  mmerge . ifoldr step (return start)
  where
    step x = return . consFunc x . mmerge

imap :: Monad m => (a -> m b) -> Producer m a -> Producer m b
imap func =
  ifoldr' step empty
  where
    step a bs =
      mmerge $ do
      b <- func a
      return $ cons b bs

execute :: Monad m => Producer m a -> m ()
execute = ifoldl' (const . return) ()

ifilter :: Monad m => (a -> m Bool) -> Producer m a -> Producer m a
ifilter cond =
  ifoldr' r empty
  where
    r x xs =
      mmerge $ do
      b <- cond x
      return $ if b then cons x xs else xs

toList :: (Monad m) => Producer m a -> m [a]
toList =
  ifoldr step $ return []
  where
    step x = (return . (x :) =<<)

iTakeWhile :: Monad m => (a -> Bool) -> Producer m a -> Producer m a
iTakeWhile func =
  ifoldr' r empty
  where
    r x xs = if func x then cons x xs else empty

fromList :: (Monad m) => [a] -> Producer m a
fromList = foldr cons empty

append :: Monad m => Producer m a -> Producer m a -> Producer m a
append a b = ifoldr' cons b a

iconcat :: Monad m => Producer m (Producer m a) -> Producer m a
iconcat = ifoldr' append empty

ilength :: (Monad m, Integral i) => Producer m a -> m i
ilength = ifoldl' (const . return . (+ 1))  0

itake :: (Monad m, Integral i) => i -> Producer m a -> Producer m a
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
  Producer m a -> Producer (t m) a
liftProdMonad =
  mmerge . lift . evalConsumerT (r =<< next)
  where
    r Nothing = return empty
    r (Just v) =
      return . cons v . mmerge . lift =<< processRest (r =<< next)

izipP2 :: (Monad m) =>
          Producer m v1
       -> Producer m a
       -> ConsumerT a (ConsumerT v1 (ProducerT v m)) ()
       -> Producer m v
izipP2 p1 p2 = produce .
               (`evalConsumerT` liftProdMonad p1) .
               (`evalConsumerT` (liftProdMonad . liftProdMonad) p2)

izipWith :: Monad m => (a -> b -> c) -> Producer m a -> Producer m b -> Producer m c
izipWith f p1 p2 =
    izipP2 p1 p2 $ do
        runMaybeT . forever $ liftM2 f (MaybeT $ lift next) (MaybeT next)
                              >>= lift . lift . lift . yield
        return ()

izip :: Monad m => Producer m a -> Producer m b -> Producer m (a, b)
izip = izipWith (,)
