-- Alternative module name to "List"'s @Control.Monad.Trans.List@.
-- The name @Control.Monad.Trans.List@ conflicts with "transformers"'s broken ListT.
--
-- Import this module and not @Control.Monad.Trans.List@ if using both packages.

module Control.Monad.ListT (ListT(..)) where

import Control.Monad.Trans.List (ListT(..))
