import Iterator

import Control.Monad (liftM)
import Control.Monad.Trans (lift)

testIterator :: Iterator Int IO
testIterator =
  iterator $ do
  putStrLn "hello"
  yield 1 $ do
  putStrLn "world"
  yield 10 $ do
  putStrLn "bye"
  nil 

toList :: Monad m => Iterator v m -> m [v]
toList iter =
  liftM reverse $ evalIteratesT (r []) iter
  where
    r soFar = do
      x <- next
      case x of
        Nothing -> return soFar
        Just v -> r $ v:soFar

main :: IO ()
main = do
  print =<< toList testIterator
  putStrLn ""
  print =<< evalIteratesT r testIterator
  where
    r = do
      lift . print =<< next
      lift . print =<< next
      lift . print =<< next
      lift . print =<< next
      lift . print =<< next
      return "done"

