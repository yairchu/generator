import Control.Monad
import Control.Monad.Generator
import Control.Monad.Trans.Class
import Data.List.Tree
import Data.Maybe

pythagorianTriplets :: [(Integer, Integer, Integer)]
pythagorianTriplets =
  mapMaybe fst .
  bestFirstSearchSortedChildrenOn snd .
  generate $ do
    x <- lift [1..]
    yield (Nothing, x)
    y <- lift [x..]
    yield (Nothing, x + y)
    z <- lift [y..]
    yield (Nothing, x + y + z)
    lift . guard $ x*x + y*y == z*z
    yield (Just (x, y, z), 0)

main :: IO ()
main = print $ take 10 pythagorianTriplets
