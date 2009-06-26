import Data.Iterator
import Data.Iterator.Tools

import Control.Monad (liftM)
import Control.Monad.Trans (lift)

testIterator :: Iterator Int IO
testIterator =
  iterator $ do
  putStrLn "hello"
  cons' 1 $ do
  putStrLn "world"
  cons' 10 $ do
  putStrLn "bye"
  nil

main :: IO ()
main = do
  print =<< toList testIterator
  putStrLn ""
  print =<< evalIteratesT r (imap (+ 1) testIterator)
  where
    r = do
      lift . print =<< next
      lift . print =<< next
      lift . print =<< next
      lift . print =<< next
      lift . print =<< next
      return "done"

