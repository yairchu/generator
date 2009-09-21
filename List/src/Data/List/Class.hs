{-# LANGUAGE FlexibleContexts, RankNTypes, TypeFamilies #-}

-- | The 'List' class and actions for lists

module Data.List.Class (
  -- | The List typeclass
  List (..),
  -- | List operations for MonadPlus
  cons, fromList, filter, repeat,
  -- | Standard list operations
  takeWhile, genericTake, scanl,
  transpose, zip, zipWith,
  -- | Non standard List operations
  foldrL, foldlL, toList, lengthL, lastL,
  merge2On, mergeOn,
  -- | Operations useful for monadic lists
  execute, joinM,
  -- | Convert between List types
  convList, transformListMonad, liftListMonad
  ) where

import Control.Monad (MonadPlus(..), liftM)
import Control.Monad.Identity (Identity(..))
import Control.Monad.ListT (ListT(..), ListItem(..))
import Control.Monad.Trans (MonadTrans(..))
import Data.Function (fix)
import Data.Maybe (fromJust)
import Prelude hiding (
  filter, repeat, scanl, takeWhile, zip, zipWith)

-- | A class for list types.
-- Every list has an underlying monad.
class (MonadPlus l, Monad (ItemM l)) => List l where
  type ItemM l :: * -> *
  runList :: l a -> ItemM l (ListItem l a)
  -- | Transform an action returning a list to the returned list
  --
  -- > > joinL $ Identity "hello"
  -- > "hello"
  joinL :: ItemM l (l a) -> l a

instance List [] where
  type ItemM [] = Identity
  runList [] = Identity Nil
  runList (x:xs) = Identity $ Cons x xs
  joinL = runIdentity

instance Monad m => List (ListT m) where
  type ItemM (ListT m) = m
  runList = runListT
  joinL = ListT . (>>= runList)

-- | foldr for 'List's.
-- the result and 'right side' values are monadic actions.
foldrL :: List l => (a -> ItemM l b -> ItemM l b) -> ItemM l b -> l a -> ItemM l b
foldrL consFunc nilFunc list = do
  item <- runList list
  case item of
    Nil -> nilFunc
    Cons x xs -> consFunc x (foldrL consFunc nilFunc xs)

-- | Prepend an item to a 'MonadPlus'
cons :: MonadPlus m => a -> m a -> m a
cons = mplus . return

-- | Convert a list to a 'MonadPlus'
--
-- > > fromList [] :: Maybe Int
-- > Nothing
-- > > fromList [5] :: Maybe Int
-- > Just 5
fromList :: MonadPlus m => [a] -> m a
fromList = foldr (mplus . return) mzero

-- | Convert between lists with the same underlying monad
convList :: (ItemM l ~ ItemM k, List l, List k) => l a -> k a
convList =
  joinL . foldrL step (return mzero)
  where
    step x = return . cons x . joinL

-- | filter for any MonadPlus
--
-- > > filter (> 5) (Just 3)
-- > Nothing
filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter cond =
  (>>= f)
  where
    f x
      | cond x = return x
      | otherwise = mzero

-- | An action to do foldl for 'List's
foldlL :: List l => (a -> b -> a) -> a -> l b -> ItemM l a
foldlL step startVal list = do
  item <- runList list
  case item of
    Nil -> return startVal
    Cons x xs ->
      let v = step startVal x
      in v `seq` foldlL step v xs

scanl :: List l => (a -> b -> a) -> a -> l b -> l a
scanl step startVal list =
  cons startVal . joinL $ do
    item <- runList list
    return $ case item of
      Nil -> mzero
      Cons x xs -> scanl step (step startVal x) xs

genericTake :: (Integral i, List l) => i -> l a -> l a
genericTake count list
  | count <= 0 = mzero
  | otherwise =
    joinL $ do
      item <- runList list
      return $ case item of
        Nil -> mzero
        Cons x xs -> cons x (genericTake (count-1) xs)

-- | Execute the monadic actions in a 'List'
execute :: List l => l a -> ItemM l ()
execute = foldlL const ()

-- | Transform a list of actions to a list of their results
--
-- > > joinM [Identity 4, Identity 7]
-- > [4,7]
joinM :: List l => l (ItemM l a) -> l a
joinM =
  joinL . foldrL consFunc (return mzero)
  where
    consFunc action rest =
      liftM (`cons` joinL rest) action

takeWhile :: List l => (a -> Bool) -> l a -> l a
takeWhile cond =
  joinL . foldrL step (return mzero)
  where
    step x
      | cond x = return . cons x . joinL
      | otherwise = const (return mzero)

-- | An action to transform a 'List' to a list
--
-- > > runIdentity $ toList "hello!"
-- > "hello!"
toList :: List l => l a -> ItemM l [a]
toList =
  foldrL step (return [])
  where
    step = liftM . (:)

-- | Consume a list (execute its actions) and return its length
--
-- > > runIdentity $ lengthL [1,2,3]
-- > 3
lengthL :: (Integral i, List l) => l a -> ItemM l i
lengthL = foldlL (const . (+ 1)) 0

-- | Transform the underlying monad of a list given a way to transform the monad
--
-- > > import Data.List.Tree (bfs)
-- > > bfs (transformListMonad (\(Identity x) -> [x, x]) "hey" :: ListT [] Char)
-- > "hheeeeyyyyyyyy"
transformListMonad :: (List l, List k) =>
  (forall x. ItemM l x -> ItemM k x) -> l a -> k a
transformListMonad trans =
  t . foldrL step (return mzero)
  where
    t = joinL . trans
    step x = return . cons x . t

-- | Lift the underlying monad of a list and transform it to a ListT.
--
-- Doing plain 'transformListMonad lift' instead doesn't give the compiler
-- the same knowledge about the types.
liftListMonad ::
  (MonadTrans t, Monad (t (ItemM l)), List l) =>
  l a -> ListT (t (ItemM l)) a
liftListMonad = transformListMonad lift

zip :: List l => l a -> l b -> l (a, b)
zip xx yy =
  joinL $ do
    xi <- runList xx
    case xi of
      Nil -> return mzero
      Cons x xs -> do
        yi <- runList yy
        return $ case yi of
          Nil -> mzero
          Cons y ys -> cons (x, y) (zip xs ys)

-- zipWith based on zip and not vice versa,
-- because the other way around hlint compains "use zip".
zipWith :: List l => (a -> b -> c) -> l a -> l b -> l c
zipWith func as = liftM (uncurry func) . zip as

-- | Consume all items and return the last one
--
-- > > runIdentity $ lastL "hello"
-- > 'o'
lastL :: List l => l a -> ItemM l a
lastL = liftM fromJust . foldlL (const Just) Nothing

repeat :: MonadPlus m => a -> m a
repeat = fix . cons

transpose :: List l => l (l a) -> l (l a)
transpose matrix =
  joinL $ toList matrix >>= r
  where
    r xs = do
      items <- mapM runList xs
      return $ case filter isCons items of
        [] -> mzero
        citems ->
          cons (fromList (map headL citems)) .
          joinL . r $ map tailL citems
    isCons Nil = False
    isCons _ = True

-- | Merge many lists sorted by a criteria given the criteria
--
-- > > mergeOn length [["hi", "hey", "hello"], ["cat", "falcon"], ["banana", "cucumber"]]
-- > ["hi","cat","hey","hello","banana","falcon","cucumber"]
mergeOn :: (Ord b, List l) => (a -> b) -> l (l a) -> l a
mergeOn f = joinL . foldlL (merge2On f) mzero

-- | Merge two lists sorted by a criteria given the criteria
--
-- > > merge2On id "01568" "239"
-- > "01235689"
merge2On :: (Ord b, List l) => (a -> b) -> l a -> l a -> l a
merge2On f xx yy =
  joinL $ do
    xi <- runList xx
    yi <- runList yy
    return $ case (xi, yi) of
      (Cons x xs, Cons y ys)
        | f y > f x -> cons x . merge2On f xs $ cons y ys
        | otherwise -> cons y $ merge2On f (cons x xs) ys
      (Cons x xs, Nil) -> cons x xs
      (Nil, Cons y ys) -> cons y ys
      (Nil, Nil) -> mzero

