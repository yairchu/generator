{-# LANGUAGE FlexibleContexts, TypeFamilies, DeriveTraversable #-}

-- | The 'List' class and actions for lists

module Data.List.Class (
    -- | The List typeclass
    List (..), ListItem (..), listItem,
    fromList,
    -- | List operations for MonadPlus
    cons, filter, catMaybes, mapMaybe,
    -- | List operations for Alternative
    repeat, enumFrom, enumFromTo,
    -- | Standard list operations
    take, takeWhile, genericTake, scanl, scanl1,
    transpose, zip, zipWith, tail,
    -- | Non standard List operations
    foldrL, foldlL, foldl1L, toList, lengthL, lastL,
    merge2On, mergeOn,
    -- | Operations useful for monadic lists
    execute, joinM, mapL, filterL, iterateM, takeWhileM, repeatM,
    splitAtM, splitWhenM,
    -- | Convert between List types
    transformListMonad,
    listStateJoin
    ) where

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.State (StateT(..), evalStateT, get)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity(..))
import Data.Maybe (fromJust)
import Prelude hiding (
    concat, concatMap, enumFrom, enumFromTo, filter, repeat, scanl, scanl1,
    tail, take, takeWhile, zip, zipWith)

data ListItem l a =
    Nil |
    Cons { headL :: a, tailL :: l a }
    deriving (Eq, Ord, Read, Show, Functor, Foldable, Traversable)

-- | A "catamorphism" for ListItem
listItem :: r -> (a -> l a -> r) -> ListItem l a -> r
listItem r _ Nil = r
listItem _ f (Cons h t) = f h t

infixr 5 `cons`

cons :: Alternative f => a -> f a -> f a
cons = (<|>) . pure

-- | A class for list types.
-- Every list has an underlying monad.
class (Alternative l, Monad (ItemM l)) => List l where
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

-- A "monadic-catamorphism" for lists.
-- Unlike folds, this only looks at the list head.
--
-- Should this be exposed? Needs a good name first..
deconstructList :: List l => ItemM l r -> (a -> l a -> ItemM l r) -> l a -> ItemM l r
deconstructList onNil onCons list =
    runList list >>= listItem onNil onCons

deconstructList' :: List l => l r -> (a -> l a -> l r) -> l a -> l r
deconstructList' onNil onCons =
    joinL . deconstructList (pure onNil) onCons'
    where
        onCons' x = pure . onCons x

-- | foldr for 'List's.
-- the result and 'right side' values are monadic actions.
foldrL :: List l => (a -> ItemM l b -> ItemM l b) -> ItemM l b -> l a -> ItemM l b
foldrL consFunc nilFunc =
    deconstructList nilFunc onCons
    where
        onCons x = consFunc x . foldrL consFunc nilFunc

-- | Convert a list to a 'List'
--
-- > > fromList [] :: Maybe Int
-- > Nothing
-- > > fromList [5] :: Maybe Int
-- > Just 5
fromList :: List l => [a] -> l a
fromList = foldr cons empty

-- | filter for any MonadPlus
--
-- > > filter (> 5) (Just 3)
-- > Nothing
filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter cond =
    (>>= f)
    where
        f x
            | cond x = pure x
            | otherwise = mzero

-- | An action to do foldl for 'List's
foldlL :: List l => (a -> b -> a) -> a -> l b -> ItemM l a
foldlL step startVal =
    deconstructList (pure startVal) onCons
    where
        onCons x xs =
            let v = step startVal x
            in v `seq` foldlL step v xs

foldl1L :: List l => (a -> a -> a) -> l a -> ItemM l a
-- should use "error" or "fail"?
foldl1L = deconstructList (error "foldl1L: empty list") . foldlL

scanl :: List l => (a -> b -> a) -> a -> l b -> l a
scanl step startVal =
    cons startVal . deconstructList' empty (scanl step . step startVal)

scanl1 :: List l => (a -> a -> a) -> l a -> l a
scanl1 = deconstructList' empty . scanl

genericTake :: (Integral i, List l) => i -> l a -> l a
genericTake count
    | count <= 0 = const empty
    | otherwise = deconstructList' empty onCons
    where
        onCons x = cons x . genericTake (count - 1)

take :: List l => Int -> l a -> l a
take = genericTake

-- | Execute the monadic actions in a 'List'
execute :: List l => l a -> ItemM l ()
execute = foldlL const ()

-- | Transform a list of actions to a list of their results
--
-- > > joinM [Identity 4, Identity 7]
-- > [4,7]
joinM :: List l => l (ItemM l a) -> l a
joinM =
    joinL . foldrL consFunc (pure empty)
    where
        consFunc action rest = fmap (`cons` joinL rest) action

mapL :: List l => (a -> ItemM l b) -> l a -> l b
mapL func = joinM . fmap func

takeWhile :: List l => (a -> Bool) -> l a -> l a
takeWhile = takeWhileM . fmap pure

repeatM :: List l => ItemM l a -> l a
repeatM = joinM . repeat

filterL :: List l => (a -> ItemM l Bool) -> l a -> l a
filterL cond =
    joinL . foldrL step (pure empty)
    where
        step x rest = do
            b <- cond x
            if b
                then pure . cons x . joinL $ rest
                else rest

takeWhileM :: List l => (a -> ItemM l Bool) -> l a -> l a
takeWhileM cond =
    joinL . foldrL step (pure empty)
    where
        step x rest = do
            b <- cond x
            if b
                then pure . cons x . joinL $ rest
                else pure empty

-- | An action to transform a 'List' to a list
--
-- > > runIdentity $ toList "hello!"
-- > "hello!"
toList :: List l => l a -> ItemM l [a]
toList =
    foldrL step (pure [])
    where
        step = fmap . (:)

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
    (ItemM l (k a) -> ItemM k (k a)) -> l a -> k a
transformListMonad trans =
    t . foldrL step (pure empty)
    where
        t = joinL . trans
        step x = pure . cons x . t

zip :: List l => l a -> l b -> l (a, b)
zip xx yy =
    deconstructList' empty onConsX xx
    where
        onConsX x xs = deconstructList' empty (onConsXY x xs) yy
        onConsXY x xs y ys = cons (x, y) $ zip xs ys

-- zipWith based on zip and not vice versa,
-- because the other way around hlint compains "use zip".
zipWith :: List l => (a -> b -> c) -> l a -> l b -> l c
zipWith func as = fmap (uncurry func) . zip as

tail :: List l => l a -> l a
tail = joinL . fmap tailL . runList

-- | Consume all items and return the last one
--
-- > > runIdentity $ lastL "hello"
-- > 'o'
lastL :: List l => l a -> ItemM l a
lastL = fmap fromJust . foldlL (const Just) Nothing

repeat :: Alternative f => a -> f a
repeat = fix . cons

transpose :: List l => l (l a) -> l (l a)
transpose matrix =
    joinL $ toList matrix >>= r
    where
        r xs =
            traverse runList xs <&>
            \items ->
            case filter isCons items of
            [] -> empty
            citems ->
                cons (fromList (citems <&> headL)) .
                joinL . r $ citems <&> tailL
        isCons Nil = False
        isCons _ = True

-- | Merge many lists sorted by a criteria given the criteria
--
-- > > mergeOn length [["hi", "hey", "hello"], ["cat", "falcon"], ["banana", "cucumber"]]
-- > ["hi","cat","hey","hello","banana","falcon","cucumber"]
mergeOn :: (Ord b, List l) => (a -> b) -> l (l a) -> l a
mergeOn f = joinL . foldlL (merge2On f) empty

-- | Merge two lists sorted by a criteria given the criteria
--
-- > > merge2On id "01568" "239"
-- > "01235689"
merge2On :: (Ord b, List l) => (a -> b) -> l a -> l a -> l a
merge2On f xx yy =
    joinL $ do
        xi <- runList xx
        yi <- runList yy
        pure $ case (xi, yi) of
            (Cons x xs, Cons y ys)
                | f y > f x -> cons x . merge2On f xs $ cons y ys
                | otherwise -> cons y $ merge2On f (cons x xs) ys
            (Cons x xs, Nil) -> cons x xs
            (Nil, Cons y ys) -> cons y ys
            (Nil, Nil) -> empty

-- | Monadic version of iterate.
-- Can be used to produce trees given a children of node function.
--
-- > import Data.List.Tree (bfsLayers)
-- > take 3 $ bfsLayers (iterateM (\i -> [i*2, i*2+1]) [1] :: ListT [] Int)
-- > [[1],[2,3],[4,5,6,7]]
iterateM :: List l => (a -> ItemM l a) -> ItemM l a -> l a
iterateM step startM =
    joinL (startM <&> \start -> cons start . iterateM step . step $ start)

-- | Monadic variant of splitAt.
-- Consumes x items from the list and return them with the remaining monadic list.
splitAtM :: List l => Int -> l a -> ItemM l ([a], l a)
splitAtM at list
    | at <= 0 = pure ([], list)
    | otherwise = do
        item <- runList list
        case item of
            Nil -> pure ([], empty)
            Cons x xs -> do
                (pre, post) <- splitAtM (at-1) xs
                pure (x:pre, post)

-- | Monadic variant of break.
-- Consumes items from the list until a condition holds.
splitWhenM :: List l => (a -> ItemM l Bool) -> l a -> ItemM l ([a], l a)
splitWhenM cond list = do
    item <- runList list
    case item of
        Nil -> pure ([], empty)
        Cons x xs -> do
            isSplit <- cond x
            if isSplit
                then pure ([], cons x xs)
                else do
                    (pre, post) <- splitWhenM cond xs
                    pure (x:pre, post)

-- | listStateJoin can transform a
-- @ListT (StateT s m) a@ to a @StateT s m (ListT m a)@.
--
-- When iterating a list, a state is already maintained and passed along
-- in the form of the location along the list.
-- This joins the inner @StateT s@ into the list.
-- The list will fork the state given to it and won't share its changes.
listStateJoin :: (List l, List k, ItemM l ~ StateT s (ItemM k))
    => l a -> ItemM l (k a)
listStateJoin list =
    get <&>
    \start -> joinL . (`evalStateT` start) $ deconstructList (pure empty) onCons list
    where
        onCons x = fmap (cons x) . listStateJoin

catMaybes :: MonadPlus m => m (Maybe a) -> m a
catMaybes = (>>= maybe empty pure)

mapMaybe :: MonadPlus m => (a -> Maybe b) -> m a -> m b
mapMaybe f = catMaybes . fmap f

enumFrom :: (Alternative f, Enum a) => a -> f a
enumFrom x = cons x (enumFrom (succ x))

enumFromTo :: (Alternative f, Enum a) => a -> a -> f a
enumFromTo from to
    | fromEnum from > fromEnum to = empty
    | otherwise = cons from (enumFromTo (succ from) to)
