{-# OPTIONS -O2 -Wall #-}

import Control.Generator(ConsumerT, evalConsumerT, Producer, next)
import Control.Generator.ProducerT(produce, yield)
import Control.Generator.Tools(execute, imap, itake, toList)
import Control.Monad.Trans(MonadIO(..), lift)

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
  

main :: IO ()
main = do
 print =<< toList (itake (2::Int) intProducer)
 putStrLn ""
 execute $ imap print intProducer
 evalConsumerT testConsumer intProducer
 evalConsumerT (evalConsumerT testConsumer2 strProducer) intProducer
