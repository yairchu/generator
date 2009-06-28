{-# OPTIONS -O2 -Wall #-}

module Control.Generator.ProducerT(
  ProducerT, produce, yield
  ) where

import Control.Generator(Producer, cons, empty, mmerge)
import Control.Generator.Tools(append)
import Control.Monad(join, liftM)
import Control.Monad.Trans(MonadTrans, lift)

newtype ProducerT v m a = ProducerT (m (Producer v m, m a))

instance Monad m => Monad (ProducerT v m) where
  return = ProducerT . return . ((,) empty) . return
  (ProducerT a) >>= f =
    ProducerT $ do
    (prodA, monA) <- a
    valA <- monA
    let ProducerT r = f valA
        prod = append prodA . mmerge . liftM fst $ r
        mon = join . liftM snd $ r
    return (prod, mon)
  fail = ProducerT . fail

instance MonadTrans (ProducerT v) where
  lift = ProducerT . return . ((,) empty)

yield :: Monad m => v -> ProducerT v m ()
yield v = ProducerT $ return (cons v empty, return ())

produce :: Monad m => ProducerT v m () -> Producer v m
produce (ProducerT prodT) =
  mmerge $ return . fst =<< prodT

