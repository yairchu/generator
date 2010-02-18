{-# LANGUAGE TypeSynonymInstances #-}
import Control.Monad.ListT
import Control.Monad.Writer
import Text.Printf

type W = Writer [String]

test :: Int -> ListT W Int
test 0 = tellL ["done"] >> mzero
test i =
  do
    tellL ["producing "++show i]
    return i `mplus` test (i-1)

instance Show a => Show (W a) where    
  show (Writer (a,w)) =
    printf "\nWriter\n log=%s\n value=%s" (show w) (show a)
    
    
main = do
  print (listenL (test 5))
  putStrLn "---"
  print (execWriterL (test 5))
