{-# OPTIONS -O2 -Wall #-}

import Control.Generator(Producer)
import Control.Generator.ProducerT(produce, yield)
import Control.Generator.Tools(execute, imap, itake, toList)
import Control.Monad.Trans(lift)

test :: Producer Int IO
test = produce $ do
         lift $ putStrLn "hello"
         yield 1
         lift $ putStrLn "world"
         yield 10
         lift $ putStrLn "bye"
         yield 100
         lift $ putStrLn "moo"

main :: IO ()
main = do
 print =<< toList (itake (2::Int) test)
 putStrLn ""
 execute $ imap print test
