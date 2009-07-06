{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE Rank2Types #-}

module Control.Generator.Tools(
  execute, fromList, iconcat,
  ifoldl, ifoldl', ifoldr, ifoldr', ilast, ilength, imap,
  ifilter, iscanl, itake, iTakeWhile, izip, izipWith, izipP2,
  liftProdMonad, toList, transformProdMonad
  ) where

import Control.Generator (
  ConsumerT, Producer, append, cons, empty,
  evalConsumerT, mmerge, next, processRest, singleItem)
import Control.Generator.ProducerT (ProducerT, produce, yield)
import Control.Monad (forever, liftM, liftM2)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (StateT(..), evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Function (fix)

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
  evalConsumerT . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> lift nilFunc
      Just x -> lift . consFunc x =<< processRest rest

-- for operations that build Producers, combine step with the mmerge etc boiler-plate
ifoldr' :: Monad m => (b -> Producer m a -> Producer m a) -> Producer m a -> Producer m b -> Producer m a
ifoldr' consFunc start =
  mmerge . ifoldr step (return start)
  where
    step x = return . consFunc x . mmerge

singleItemM :: Monad m => m a -> Producer m a
singleItemM = mmerge . liftM singleItem

consM :: Monad m => m a -> Producer m a -> Producer m a
consM = append . singleItemM

imap :: Monad m => (a -> m b) -> Producer m a -> Producer m b
imap func = ifoldr' (consM . func) empty

execute :: Monad m => Producer m a -> m ()
execute = ifoldl' (const . return) ()

toList :: (Monad m) => Producer m a -> m [a]
toList =
  ifoldr step $ return []
  where
    step x = (return . (x :) =<<)

-- used in ifilter and iTakeWhile
ifilterStep :: Monad m => (a -> m Bool) -> (Producer m a -> Producer m a) -> a -> Producer m a -> Producer m a
ifilterStep cond onFalse x xs =
  mmerge $ do
  b <- cond x
  return $ if b then cons x xs else onFalse xs

ifilter :: Monad m => (a -> m Bool) -> Producer m a -> Producer m a
ifilter cond =
  ifoldr' (ifilterStep cond id) empty

iTakeWhile :: Monad m => (a -> m Bool) -> Producer m a -> Producer m a
iTakeWhile cond =
  ifoldr' (ifilterStep cond (const empty)) empty

fromList :: (Monad m) => [a] -> Producer m a
fromList = foldr cons empty

iconcat :: Monad m => Producer m (Producer m a) -> Producer m a
iconcat = ifoldr' append empty

ilength :: (Monad m, Integral i) => Producer m a -> m i
ilength = ifoldl' (const . return . (+ 1))  0

itake :: (Monad m, Integral i) => i -> Producer m a -> Producer m a
itake count =
  mmerge . evalConsumerT (foldr r (return empty) [1..count])
  where
    r _ rest = do
      mx <- next
      case mx of
        Nothing -> return empty
        Just x ->
          return . cons x . mmerge =<< processRest rest

maybeForever :: Monad m => MaybeT m a -> m ()
maybeForever = (>> return ()) . runMaybeT . forever

ilast :: Monad m => Producer m a -> m a
ilast =
  evalConsumerT $ do
  Just x <- next
  liftM snd . (`runStateT` x) . maybeForever $ MaybeT (lift next) >>= put

transformProdMonad :: (Monad m, Monad s) =>
  (forall a. m a -> m (s a)) -> Producer m v -> m (Producer s v)
transformProdMonad trans =
  evalConsumerT . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> return empty
      Just x ->
        liftM (cons x . mmerge) $
        processRest rest >>= lift . trans

liftProdMonad ::
  (Monad (t m), Monad m, MonadTrans t) =>
  Producer m a -> Producer (t m) a
liftProdMonad = mmerge . lift . transformProdMonad (return . lift)

consumeLift ::
  (Monad (t m), Monad m, MonadTrans t) =>
  ConsumerT a (t m) b -> Producer m a -> t m b
consumeLift consumer = evalConsumerT consumer . liftProdMonad

izipP2 :: (Monad m) =>
          Producer m a
       -> Producer m b
       -> ConsumerT b (ConsumerT a (ProducerT r m)) ()
       -> Producer m r
izipP2 p1 p2 =
  produce . (`consumeLift` p1) . (`consumeLift` liftProdMonad p2)

izip :: Monad m => Producer m a -> Producer m b -> Producer m (a, b)
izip p1 p2 =
  izipP2 p1 p2 . maybeForever $
  liftM2 (,) (MaybeT (lift next)) (MaybeT next) >>=
  lift . lift . lift . yield

izipWith :: Monad m => (a -> b -> m c) -> Producer m a -> Producer m b -> Producer m c
izipWith f p = imap (uncurry f) . izip p

iscanl :: Monad m => (a -> b -> m a) -> a -> Producer m b -> Producer m a
iscanl func start =
  produce . consumeLift (evalStateT r start)
  where
    r =
      maybeForever $ do
      s <- get
      lift . lift . lift $ yield s
      x <- MaybeT $ lift next
      put =<< (lift . lift . lift . lift) (func s x)

