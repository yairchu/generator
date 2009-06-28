{-# OPTIONS -O2 -Wall #-}

import Control.Generator (ConsumerT, evalConsumerT, Producer, next)
import Control.Generator.ProducerT (ProducerT, produce, yield)
import Control.Generator.Tools (execute, imap, itake, toList)
import Control.Monad.Trans (MonadIO(..), lift)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad (forever)

intProducer :: MonadIO m => Producer Int m
intProducer =
    produce $ do
      liftIO $ putStrLn "yielding 1"
      yield 1
      liftIO $ putStrLn "yielding 10"
      yield 10
      liftIO $ putStrLn "yielding 100"
      yield 100
      liftIO $ putStrLn "int producer is done!"

strProducer :: MonadIO m => Producer String m
strProducer =
    produce $ do
      liftIO $ putStrLn "yielding string1"
      yield "string1"
      liftIO $ putStrLn "yielding string2"
      yield "string2"
      liftIO $ putStrLn "string producer is done!"

testConsumer :: ConsumerT Int IO ()
testConsumer = do
  lift . putStrLn $ "testConsumer starting" 
  Just a <- next
  Just b <- next
  lift . putStrLn $ "read two values whose sum is " ++ show (a+b)

-- A complicated ConsumerT transformer stack should probably get a
-- newtype with names for the various lifters... But for the example's
-- sake, we keep fewer levels of indirection
testConsumer2 :: ConsumerT String (ConsumerT Int IO) ()
testConsumer2 = do
  liftIO . putStrLn $ "testConsumer2 starting" 
  Just s1 <- next
  Just i1 <- lift next
  liftIO . putStrLn $ "We can interlace actions between consumptions, of course :-)"
  Just i2 <- lift next
  Just s2 <- next
  Nothing <- next
  liftIO . putStrLn $ "read two int values whose sum is " ++ show (i1+i2)
  liftIO . putStrLn $ "read two str values whose concat is " ++ s1 ++ s2

cumSum :: Monad m => ConsumerT Int (ProducerT Int m) ()
cumSum =
  r 0
  where
    r s = do
      lift $ yield s
      v <- next
      maybe (return ()) (r . (s +)) v

izip :: Monad m => ConsumerT a (ConsumerT b (ProducerT (a, b) m)) ()
izip = do
    runMaybeT . forever $ do
      x <- MaybeT next
      y <- MaybeT (lift next)
      lift . lift . lift . yield $ (x, y)
    return ()

lineSpace :: IO ()
lineSpace = putStrLn ""

main :: IO ()
main = do
  print =<< toList (itake (2::Int) intProducer)
  lineSpace
  execute $ imap print intProducer
  lineSpace
  evalConsumerT testConsumer intProducer
  lineSpace
  evalConsumerT (evalConsumerT testConsumer2 strProducer) intProducer
  lineSpace
  execute . imap print . itake (3::Int) . produce $ evalConsumerT cumSum intProducer
  lineSpace
  print =<< toList (produce (evalConsumerT (evalConsumerT izip strProducer) intProducer))
