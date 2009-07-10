{-# OPTIONS -O2 -Wall #-}
{-# LANGUAGE Rank2Types #-}

-- | Fold `Producer`s
module Control.Generator.Folds (
  -- | Classic folds
  foldlP, foldrP, filterP, scanlP,
  takeWhileP,
  -- | List-like operations
  lastP, lengthP, takeP,
  fromList, toList, execute,
  -- | Zips
  zipP, zipWithMP,
  -- | Esoteric folds
  foldlMP, foldlP', foldrP', mapMP,
  liftProdMonad, transformProdMonad, consumeLift
  ) where

import Control.Generator.Consumer (
  ConsumerT, evalConsumerT, next, consumeRestM)
import Control.Monad.Producer (Producer, cons, consM, empty, joinP)
import Control.Monad.Generator (produce, yield)
import Control.Monad (forever, liftM, liftM2)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Function (fix)

-- | Left-fold for 'Producer' with function returning a monadic action holding the result.
foldlMP :: Monad m => (a -> b -> m a) -> a -> Producer m b -> m a
foldlMP func startVal =
  evalConsumerT $ r startVal
  where
    r s = do
      x <- next
      case x of
        Nothing -> return s
        Just v -> r =<< lift (func s v)

-- | Left-fold for 'Producer'
foldlP :: Monad m => (a -> b -> a) -> a -> Producer m b -> m a
foldlP func =
  foldlMP func'
  where
    func' x = return . func x

-- | Left-fold for 'Producer' which reduces intermediate values to WHNF (weak head normal form).
foldlP' :: Monad m => (a -> b -> a) -> a -> Producer m b -> m a
foldlP' func startVal =
  evalConsumerT $ r startVal
  where
    r s = do
      x <- s `seq` next
      case x of
        Nothing -> return s
        Just v -> r $ func s v

-- | Right-fold for 'Producer'.
-- The "cons" function gets a monadic action holding the right result so it could choose to avoid its execution.
foldrP :: Monad m => (a -> m b -> m b) -> m b -> Producer m a -> m b
foldrP consFunc nilFunc =
  evalConsumerT . fix $ \rest -> do
    mx <- next
    case mx of
      Nothing -> lift nilFunc
      Just x -> lift . consFunc x =<< consumeRestM rest

-- | Right-fold for making a 'Producer' from a 'Producer'
foldrP' :: Monad m => (b -> Producer m a -> Producer m a) -> Producer m a -> Producer m b -> Producer m a
foldrP' consFunc start =
  joinP . foldrP step (return start)
  where
    step x = return . consFunc x . joinP

-- | Map with a function that returns a monadic action holding the result.
-- For a non-monadic variant use 'fmap'
mapMP :: Monad m => (a -> m b) -> Producer m a -> Producer m b
mapMP func = foldrP' (consM . func) empty

-- | Execute all the monadic actions of a 'Producer'
execute :: Monad m => Producer m a -> m ()
execute = foldlP' const ()

-- | Transform a 'Producer' to a list, executing its monadic actions in the process.
toList :: (Monad m) => Producer m a -> m [a]
toList =
  foldrP step $ return []
  where
    step x = (return . (x :) =<<)

-- used in filterP and takeWhileP
filterStepP :: Monad m => (a -> Bool) -> (Producer m a -> Producer m a) -> a -> Producer m a -> Producer m a
filterStepP cond onFalse x xs
  | cond x = cons x xs
  | otherwise = onFalse xs

-- | Filter a 'Producer'
filterP :: Monad m => (a -> Bool) -> Producer m a -> Producer m a
filterP cond =
  foldrP' (filterStepP cond id) empty

-- | Take values from the 'Producer' while a condition is satisfied
takeWhileP :: Monad m => (a -> Bool) -> Producer m a -> Producer m a
takeWhileP cond =
  foldrP' (filterStepP cond (const empty)) empty

-- | Create a 'Producer' from a list of values.
-- The resulting producer will not have any monadic effects.
fromList :: (Monad m) => [a] -> Producer m a
fromList = foldr cons empty

-- | Execute a 'Producer's monadic actions and return its length
lengthP :: (Monad m, Integral i) => Producer m a -> m i
lengthP = foldlP' (const . (+ 1))  0

-- | Take the first N elements from a 'Producer'
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

-- | Execute a 'Producer' and return its last value.
-- Especially useful for getting the leafs of a tree represented by a 'Producer []'
lastP :: Monad m => Producer m a -> m a
lastP = foldlP' (const id) undefined

-- | Transform the monad of a 'Producer' given a function to transform the monad.
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

-- | Lift the monad of a 'Producer'
liftProdMonad ::
  (Monad (t m), Monad m, MonadTrans t) =>
  Producer m a -> Producer (t m) a
liftProdMonad = joinP . lift . transformProdMonad (return . lift)

consumeLift ::
  (Monad (t m), Monad m, MonadTrans t) =>
  ConsumerT a (t m) b -> Producer m a -> t m b
consumeLift consumer = evalConsumerT consumer . liftProdMonad

-- | Zip two 'Producer's.
-- Also "zips"/interlaces their monadic effects.
zipP :: Monad m => Producer m a -> Producer m b -> Producer m (a, b)
zipP p1 p2 =
  produce . (`consumeLift` p1) . (`consumeLift` liftProdMonad p2) .
  maybeForever $
  liftM2 (,) (MaybeT (lift next)) (MaybeT next) >>=
  lift . lift . lift . yield

-- | Zip two 'Producer's and transform their values.
zipWithMP :: Monad m => (a -> b -> m c) -> Producer m a -> Producer m b -> Producer m c
zipWithMP f p = mapMP (uncurry f) . zipP p

-- | scanl for 'Producer'
scanlP :: Monad m => (a -> b -> a) -> a -> Producer m b -> Producer m a
scanlP func start =
  produce . consumeLift (evalStateT r start)
  where
    r =
      maybeForever $ do
      s <- get
      lift . lift . lift $ yield s
      x <- MaybeT $ lift next
      put $ func s x

