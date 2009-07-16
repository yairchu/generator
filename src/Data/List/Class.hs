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
  foldlL, toList, lengthL, lastL,
  mergeOn, merge2On,
  -- | Operations useful for monadic lists
  execute, joinM,
  -- | Convert between List types
  convList, transformListMonad, liftListMonad
  ) where

import Control.Monad (MonadPlus(..), ap, join, liftM)
import Control.Monad.Identity (Identity(..))
import Control.Monad.ListT (ListT(..), ListItem(..), foldrListT)
import Control.Monad.Trans (MonadTrans(..))
import Data.Function (fix)
import Prelude hiding (
  filter, repeat, scanl, takeWhile, zip, zipWith)

-- | A class for list types.
-- Every list has an underlying monad.
class (MonadPlus l, Monad (ItemM l)) => List l where
  type ItemM l :: * -> *
  -- | Transform an action returning a list to the returned list
  --
  -- > > joinL $ Identity "hello"
  -- > "hello"
  joinL :: ItemM l (l b) -> l b
  -- | foldr for 'List's.
  -- the result and 'right side' values are monadic actions.
  foldrL :: (a -> ItemM l b -> ItemM l b) -> ItemM l b -> l a -> ItemM l b
  foldrL consFunc nilFunc = foldrL consFunc nilFunc . toListT
  -- | Convert to a 'ListT'.
  --
  -- Can be done with a foldrL but included in type-class for efficiency.
  toListT :: l a -> ListT (ItemM l) a
  toListT = convList
  -- | Convert from a 'ListT'.
  --
  -- Can be done with a foldrL but included in type-class for efficiency.
  fromListT :: ListT (ItemM l) a -> l a
  fromListT = convList

instance List [] where
  type ItemM [] = Identity
  joinL = runIdentity
  foldrL = foldr
  toListT = fromList

instance Monad m => List (ListT m) where
  type ItemM (ListT m) = m
  joinL = ListT . (>>= runListT)
  foldrL = foldrListT
  toListT = id
  fromListT = id

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

-- for foldlL and scanl
foldlL' :: List l =>
  (a -> (ItemM l) c -> c) -> (a -> c) ->
  (a -> b -> a) -> a -> l b -> c
foldlL' joinVals atEnd step startVal =
  t startVal . foldrL astep (return atEnd)
  where
    astep x rest = return $ (`t` rest) . (`step` x)
    t cur = joinVals cur . (`ap` return cur)

-- | An action to do foldl for 'List's
foldlL :: List l => (a -> b -> a) -> a -> l b -> ItemM l a
foldlL step startVal =
  foldlL' (const join) id astep (return startVal)
  where
    astep rest x = liftM (`step` x) rest

scanl :: List l => (a -> b -> a) -> a -> l b -> l a
scanl =
  foldlL' consJoin $ const mzero
  where
    consJoin cur = cons cur . joinL

genericTake :: (Integral i, List l) => i -> l a -> l a
genericTake count
  | count <= 0 = const mzero
  | otherwise = foldlL' joinStep (const mzero) next Nothing
  where
    next Nothing x = Just (count, x)
    next (Just (i, _)) y = Just (i - 1, y)
    joinStep Nothing = joinL
    joinStep (Just (1, x)) = const $ return x
    joinStep (Just (_, x)) = cons x . joinL

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
    consFunc action rest = do
      x <- action
      return . cons x . joinL $ rest

takeWhile :: List l => (a -> Bool) -> l a -> l a
takeWhile cond =
  joinL . foldrL step (return mzero)
  where
    step x
      | cond x = return . cons x . joinL
      | otherwise = const $ return mzero

-- | An action to transform a 'List' to a list
--
-- > > runIdentity $ toList "hello!"
-- > "hello!"
toList :: List l => l a -> ItemM l [a]
toList =
  foldrL step $ return []
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
zip as bs =
  r0 (toListT as) (toListT bs)
  where
    r0 xx yy =
      joinL $ do
        xi <- runListT xx
        case xi of
          Nil -> return mzero
          Cons x xs -> r1 x xs yy
    r1 :: List l => a -> ListT (ItemM l) a -> ListT (ItemM l) b -> ItemM l (l (a, b))
    r1 x xs yy = do
      yi <- runListT yy
      return $ case yi of
        Nil -> mzero
        Cons y ys ->
          cons (x, y) $ r0 xs ys

-- zipWith based on zip and not vice versa,
-- because the other way around hlint compains "use zip".
zipWith :: List l => (a -> b -> c) -> l a -> l b -> l c
zipWith func as = liftM (uncurry func) . zip as

-- | Consume all items and return the last one
--
-- > > runIdentity $ lastL "hello"
-- > 'o'
lastL :: List l => l a -> ItemM l a
lastL = foldlL (const id) undefined

repeat :: MonadPlus m => a -> m a
repeat = fix . cons

transpose :: List l => l (l a) -> l (l a)
transpose matrix =
  joinL $ toList matrix >>= r . map toListT
  where
    r xs = do
      items <- mapM runListT xs
      return $ case filter isCons items of
        [] -> mzero
        citems ->
          cons (fromList (map headL citems)) .
          joinL . r $ map tailL citems
    isCons Nil = False
    isCons _ = True

mergeOn :: (Ord b, List l) => (a -> b) -> l (l a) -> l a
mergeOn f = joinL . foldlL (merge2On f) mzero

merge2On :: (Ord b, List l) => (a -> b) -> l a -> l a -> l a
merge2On f xx yy =
  fromListT . joinL $ do
    xi <- runListT (toListT xx)
    yi <- runListT (toListT yy)
    return $ case (xi, yi) of
      (Cons x xs, Cons y ys)
        | f y > f x -> cons x . merge2On f xs $ cons y ys
        | otherwise -> cons y $ merge2On f (cons x xs) ys
      (Cons x xs, Nil) -> cons x xs
      (Nil, Cons y ys) -> cons y ys
      (Nil, Nil) -> mzero

