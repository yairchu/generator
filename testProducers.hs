{-# OPTIONS -O2 -Wall #-}

import Control.Generator (ConsumerT, empty, evalConsumerT, Producer, next)
import Control.Generator.ProducerT (ProducerT, produce, yield, yields)
import Control.Generator.Tools (execute, imap, itake, toList, izip, liftProdMonad)
import Control.Monad (forever, mapM_)
import Control.Monad.Maybe (MaybeT(..))
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans (MonadIO(..), lift)
import Data.Maybe (fromJust)

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
printProducer = execute . imap print

printAfterListing :: Show a => Producer IO a -> IO ()
printAfterListing p = print =<< toList p

permute :: [a] -> Producer [] a
permute [] = empty
permute xs =
  produce $ do
  i <- lift [0 .. length xs - 1]
  let (pre, x : post) = splitAt i xs
  yield x
  yields . permute $ pre ++ post

yieldExpProd :: Producer (StateT String IO) Int
yieldExpProd =
  produce . forever $ yield . length =<< lift get

yieldExpCons :: ConsumerT Int (StateT String IO) ()
yieldExpCons = do
  lift $ put "blah"
  liftIO . print . fromJust =<< next
  lift $ put "hello world"
  liftIO . print . fromJust =<< next

main :: IO ()
main =
  mapM_ (>> lineSpace)
       [printAfterListing $ itake (2::Int) intProducer
       ,printProducer intProducer
       ,evalConsumerT testConsumer intProducer
       ,evalConsumerT (evalConsumerT testConsumer2 . liftProdMonad $ strProducer) intProducer
       ,execute . imap print . itake (3::Int) . produce . evalConsumerT cumSum . liftProdMonad $ intProducer
       ,printAfterListing . izip strProducer $ intProducer
       ,print . toList $ permute "abc"
       ,evalStateT (evalConsumerT yieldExpCons yieldExpProd) "moo"
       ]
