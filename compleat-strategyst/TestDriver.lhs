This module is a test driver for `cabal test`.

We use the `quickCheckAll` function, which is written in Template Haskell
so that it can generate code after scanning this module for properties.

> {-# LANGUAGE TemplateHaskell #-}

The module name must be `Main` for `cabal build` to produce an executable,
which we named as a test suite in our `.cabal` file.

> module Main(main) where
> import Game2xNTests as G

> import Control.Monad(unless)
> import System.Exit(exitFailure)
> import Test.QuickCheck.All(quickCheckAll)

I'll credit the **leksah** project for giving me the recipe for this `main`,
as well as the entries in the `.cabal` file.

> main = do
>     allPass <- $quickCheckAll
>     unless allPass exitFailure

As I had all the properties in a different module not named
`Main`, with copious documentation, I decided to redirect to them from here.

> prop_numOpposingStrategies = G.prop_numOpposingStrategies
> prop_numStrategies         = G.prop_numStrategies
> prop_addDominant           = G.prop_addDominant
> prop_permuteStgs           = G.prop_permuteStgs
> prop_reducedSame           = G.prop_reducedSame
> prop_mixedNoSaddle         = G.prop_mixedNoSaddle
> prop_transposedValue       = G.prop_transposedValue
> prop_p1StgsSwapped         = G.prop_p1StgsSwapped

While working out this system it was convenient to have a property
guaranteed to fail. I'll leave it here but commented out.

    prop_alwaysFail = False
