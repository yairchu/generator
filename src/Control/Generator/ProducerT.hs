{-# OPTIONS -O2 -Wall #-}

module Control.Generator.ProducerT (
  ProducerT, produce, yield
  ) where

import Control.Generator (Producer, cons, empty, mmerge)
import Control.Generator.Tools (append)
import Control.Monad (join, liftM)
import Control.Monad.Trans (MonadTrans, lift)

newtype ProducerT v m a = CProducerT (m (Producer v m, m a))

instance Monad m => Monad (ProducerT v m) where
  return = CProducerT . return . ((,) empty) . return
  fail = CProducerT . fail
  (CProducerT a) >>= b =
    CProducerT $ do
    (prodA, monA) <- a
    valA <- monA
    let
      CProducerT evalB = b valA
      prod = append prodA . mmerge . liftM fst $ evalB
      mon = join . liftM snd $ evalB
    return (prod, mon)

instance MonadTrans (ProducerT v) where
  lift = CProducerT . (=<<) (return . ((,) empty) . return)

yield :: Monad m => v -> ProducerT v m ()
yield v = CProducerT $ return (cons v empty, return ())

produce :: Monad m => ProducerT v m () -> Producer v m
produce (CProducerT prodT) =
  mmerge $ return . fst =<< prodT

