import Control.Monad
import Control.Monad.Generator
import Control.Monad.Trans.Class
import Data.List.Tree
import Data.Maybe

import Control.Lens

pythagorianTriplets :: [(Integer, Integer, Integer)]
pythagorianTriplets =
    mapMaybe (^? _Right)
        . bestFirstSearchSortedChildrenOn (^? _Left)
        . generate
        $ do
            x <- lift [1 ..]
            yield (Left x)
            y <- lift [x ..]
            yield (Left (x + y))
            z <- lift [y ..]
            yield (Left (x + y + z))
            guard $ x * x + y * y == z * z
            yield (Right (x, y, z))

main :: IO ()
main = print $ take 10 pythagorianTriplets
