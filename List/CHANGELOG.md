0.6.2
----
* `ListT`'s `Functor` instance doesn't require an underlying `Monad`.

0.6.1
----
* Compatibility with Semigroup/Monoid proposal

0.6.0
----
* `ListT` only available via `Control.Monad.ListT`. Resolves clash with other packages (for inclusion in Stackage).

0.5.2
----
* `Alternative` instance

0.5.1
----
* `splitWhenM` - a monadic variant of `break`

0.5.0
----
* Add `mapMaybe`

0.4.4
----
* Temporarily remove `mapMaybe` which will require bumping major version. Its previous addition in version 0.4.3 broke the `hexpat` package which used open imports causing a name clash when it was added.

0.4.3
----
* Add `take` - a specialized version of `genericTake`
* Add `splitAtM`
* Add `catMaybe`
* Add `mapMaybe` (temporarily removed in 0.4.4)

0.4.2
----
* `cons` moved to List class so one could override with faster implementations
* Add `enumFrom`
* Add `enumFromTo`
* Add `tail`
* Add `filterL`

0.4.1
----
* `Control.Monad.Trans.List.Funcs`: List functions specialized to `ListT` (to tell type inference what type is used)
* Avoid using `RankNTypes`
* `cons` is a right-associative operator
* `ListT` also available on `Control.Monad.Trans.List` (reverted in 0.6.0)
* Add `concat` (different from `join` in that inner lists are pure lists)
* Add `concatMap` (different from `(=<<)` in that inner lists are pure lists)
* Add `scanl1`
* Add `repeatM`

0.4.0
----
* Re-introduce `joinM` due to use-cases in `hexpat`
* Add `mapL`

0.3.0
----
* Add minor version number according to the package versioning policy.
* Use `transformers` instead of `mtl`
* Expose `ListT`'s data constructor
* `joinM` removed. Use `(>>= lift)` instead. (re-introduced in 0.4.0)
* `Functor` instance for `ListItem`
* `listStateJoin` - embeds `StateT` inside the list.
* Add `takeWhile`
* Add `sortOn`
* Add `iterateM`
* Add `foldl1L`

0.2
----
* Add instances for `Eq`, `Ord`, `Read`, `Show`
* `foldrListT'` generalized to `foldrL'
* `List` class independent of `ListT` - `toListT` and `fromListT` class functions removed.
* Add `foldrL`

0.1
----
* Initial version
