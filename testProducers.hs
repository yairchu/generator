{-# OPTIONS -O2 -Wall #-}

import Control.Monad.ListT (ListT)
import Control.Monad.Consumer (ConsumerT, next)
import Control.Monad.Producer (Producer, consume)
import Control.Monad.Generator (GeneratorT, produce, yield, yields)
import Control.Monad ( {- forever, -} mapM_, mzero)
--import Control.Monad.Maybe (MaybeT(..))
--import Control.Monad.State (evalStateT, get, modify)
import Control.Monad.Trans (MonadIO(..), lift)
import Data.List.Class (fromList, genericTake, execute, sequence_, toList)

import Prelude hiding (sequence_)

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

testConsumer :: ConsumerT Int (ListT IO) IO ()
testConsumer = do
  lift . putStrLn $ "testConsumer starting"
  Just a <- next
  Just b <- next
  lift . putStrLn $ "read two values whose sum is " ++ show (a+b)

-- A complicated ConsumerT transformer stack should probably get a
-- newtype with names for the various lifters... But for the example's
-- sake, we keep fewer levels of indirection
{-
testConsumer2 :: ConsumerT String (Producer (ConsumerT Int (Producer IO) IO)) (ConsumerT Int (Producer IO) IO) ()
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
-}

{-
cumSum :: Monad m => ConsumerT Int (GeneratorT Int m) ()
cumSum = do
  runMaybeT . flip evalStateT 0 . forever $ do
    lift . lift . lift . yield =<< get
    modify . (+) =<< lift (MaybeT next)
  return ()
-}

lineSpace :: IO ()
lineSpace = putStrLn ""

printProducer :: Show a => Producer IO a -> IO ()
printProducer = sequence_ . fmap print

permute :: [a] -> Producer [] a
permute [] = mzero
permute xs =
  produce $ do
  i <- lift [0 .. length xs - 1]
  let (pre, x : post) = splitAt i xs
  yield x
  yields . permute $ pre ++ post

transTest :: ListT IO (IO ())
transTest = do
  lift $ putStrLn "Hello"
  lift $ putStrLn "World"
  a <- fromList "ABC"
  lift $ print a
  return $ print a

main :: IO ()
main =
  mapM_ (>> lineSpace)
       [execute transTest
       ,printProducer intProducer
       ,printProducer . genericTake (2::Int) $ intProducer
       ,consume testConsumer intProducer
       --,consume (consume testConsumer2 . liftProdMonad $ strProducer) intProducer
       --,execute . fmap print . takeL (3::Int) . produce . evalConsumerT cumSum . liftProdMonad $ intProducer
       --,printProducer . zipP strProducer $ intProducer
       ,print . toList $ permute "abc"
       ]

