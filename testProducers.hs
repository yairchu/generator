{-# OPTIONS -O2 -Wall #-}

import Control.Generator.Consumer (ConsumerT, evalConsumerT, next)
import Control.Generator.Producer (Producer, empty)
import Control.Generator.Memo (memo)
import Control.Generator.ProducerT (ProducerT, produce, yield, yields)
import Control.Generator.Tools (execute, mapP, takeP, toList, zipP, liftProdMonad)
import Control.Monad (forever, mapM_)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.Trans (MonadIO(..), lift)

intProducer :: Producer IO Int
intProducer =
    produce $ do
      lift . putStrLn $ "yielding 1"
      yield 1
      lift . putStrLn $ "yielding 10"
      yield 10
      lift . putStrLn $ "yielding 100"
      yield 100
      lift . putStrLn $ "int producer is done!"

strProducer :: Producer IO String
strProducer =
    produce $ do
      lift . putStrLn $ "yielding string1"
      yield "string1"
      lift . putStrLn $ "yielding string2"
      yield "string2"
      lift . putStrLn $ "string producer is done!"

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
cumSum = do
  runMaybeT . flip evalStateT 0 . forever $ do
    lift . lift . lift . yield =<< get
    modify . (+) =<< lift (MaybeT next)
  return ()

lineSpace :: IO ()
lineSpace = putStrLn ""

printProducer :: Show a => Producer IO a -> IO ()
printProducer = execute . mapP print

permute :: [a] -> Producer [] a
permute [] = empty
permute xs =
  produce $ do
  i <- lift [0 .. length xs - 1]
  let (pre, x : post) = splitAt i xs
  yield x
  yields . permute $ pre ++ post

memoTest :: IO ()
memoTest = do
  prod <- memo intProducer
  putStrLn "going to consume"
  printProducer prod
  printProducer prod
  printProducer prod

main :: IO ()
main =
  mapM_ (>> lineSpace)
       [printProducer intProducer
       ,printProducer . takeP (2::Int) $ intProducer
       ,evalConsumerT testConsumer intProducer
       ,evalConsumerT (evalConsumerT testConsumer2 . liftProdMonad $ strProducer) intProducer
       ,execute . mapP print . takeP (3::Int) . produce . evalConsumerT cumSum . liftProdMonad $ intProducer
       ,printProducer . zipP strProducer $ intProducer
       ,print . toList $ permute "abc"
       ,memoTest
       ]

