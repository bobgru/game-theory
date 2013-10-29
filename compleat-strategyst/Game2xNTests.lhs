Testing Solutions of 2 x N Games 
--------------------------------

> module Game2xNTests where
> import Game2xN
> import Data.List(sort)
> import Test.QuickCheck

**Property-based Tests**

Did we get the implementation right? If so, we should be able to
see certain properties manifested, such as:
* A solution doesn't change when a game is augmented with a dominant strategy.
* A solution to a game with a dominant strategy doesn't change when that
  strategy is eliminated.
* A solution doesn't change when player 2's strategies are rearranged.
* The solution to a 2 x _n_ is the same as that of the 2 x 2 game returned
  with it.
* A game with a mixed meta-strategy does not have a saddlepoint.
* The value of a 2 x 2 game is the additive inverse of the value
  of the transposed game, i.e. with players 1 and 2 switched.
* The value of a 2 x 2 game to player 1 is the same regardless which
  strategy player 2 uses, and vice versa.
* The value of a game doesn't change when player 1's strategies are swapped.
* The value of a 2 x _n_ game to player 1 is the best among all 2 x 2 games
  created from pairs of player 2's strategies.

**Generating Random Games**

At the heart of property-based testing is the generation of random data,
which here means the payoff matrix. We'll start with a single strategy
generator, intended to produce a column of the payoff matrix, along with
a simple property to exercise it.

> strategy :: Int -> Int -> Gen Strategy
> strategy i n = do
>     s <- vector n
>     return (i, s)

The `prop_numOpposingStrategies` function asserts that for all lengths
between 2 and 10 the `strategy` function generates a strategy with
the requested length. The length of a strategy is the number of the
opponent's strategies.

> prop_numOpposingStrategies :: Property
> prop_numOpposingStrategies = do
>     n <- choose (2, 10)
>     collect n $ forAll (strategy 1 n) $ \s -> n == length (snd s)

Entering `quickCheck prop_numOpposingStrategies` at the GHCI
prompt produces output such as the following, where the number
of strategies of a given length is reported:

    *Game2xN> quickCheck prop_numOpposingStrategies 
    +++ OK, passed 100 tests:
    15% 10
    14% 5
    13% 6
    13% 3
    12% 7
    11% 4
     9% 8
     7% 2
     6% 9

Now we need a collection of _n_ strategies of length _m_. As we have
limited our solutions to 2 x _n_, we will always have _m_ = 2. The
`prop_numStrategies` property produces a matrix between 2 x 2 and 2 x _n_
for a random _n_ between 2 and 10, checks that the length is correct,
and that the strategy indices are 1 through _n_.

> strategies :: Int -> Int -> Gen [Strategy]
> strategies m n = mapM (\i-> strategy i m) [1..n]

> prop_numStrategies :: Property
> prop_numStrategies = do
>     n <- choose (2, 10)
>     collect n $ forAll (strategies 2 n) $ \ss -> 
>         (n == length ss)  &&  [1..n] == sort (map fst ss)

Finally, let's create a game of size 2 x _n_. We cannot use 
`mkStdGame2xN` with a list of `Strategy`, so we'll make a simple
variation that does.

> game2xN :: Int -> Gen Game2xN
> game2xN n = do
>     ps <- strategies 2 n
>     return (mkStdTestGame ps)

> mkStdTestGame ps = mkGame2xN "p1" "p2" "1-1" "1-2" nms ps
>     where nms = ["2-" ++ show i | i <- [1..length ps]]

**Testing Solutions**

* A solution doesn't change when a game is augmented with a dominant strategy.

If the solution is pure, the saddlepoint won't change when a dominant
strategy is added for player 2 because its maximum value will be higher
than at least one other strategy, and hence cannot be the minmax.
Furthermore, whatever strategy it dominates has a lower payoff in
each row, hence the added strategy does not contribute a new minimum 
for player 1 and so does not change the maxmin. 

If the solution is mixed, a dominant strategy will have been eliminated
after failure to find a saddlepoint.

> prop_addDominant = do
>     n <- choose (2, 10)
>     collect n $ forAll (game2xN n) $ checkGame
>     where
>         checkGame g = snd (solution g) == snd (solution g')
>             where
>                 g' = addP2Strategy ps "" g
>                 ps = mkDominantStg ((snd . head . payoffs) g)

The `addP2Strategy` function augments a game with a new strategy
for player 2, and its name.

> addP2Strategy :: [Int] -> String -> Game2xN -> Game2xN
> addP2Strategy ps n g = g { p2StgNames = ns', payoffs = ps' }
>     where
>         j   = length (p2StgNames g) + 1
>         ns' = p2StgNames g ++ [n]
>         ps' = payoffs g ++ [(j,ps)]

> mkDominantStg :: [Int] -> [Int]
> mkDominantStg = map (+1)

* A solution doesn't change when player 2's strategies are rearranged.

We need to permute the strategies, re-solve, and compare.

**Note** This property was falsified by two types of game:
* Those with redundant columns and pure solutions could report 
  different columns for maxmin, therefore different solutions.
* Those with redundant columns and mixed solutions could eliminate
  different dominant columns (because equality is a degenerate form of
  dominance), and therefore report different strategy numbers in the solution.

Fixing the `cmpStg` function to sort by strategy payoffs _then by strategy number_ ensured consistent behavior.

> permuteStg g = do
>     ps <- permute (payoffs g)
>     return (g { payoffs = ps })

> permute []  = return []
> permute [x] = return [x]
> permute xs = do
>     i   <- choose (0, (length xs) - 1)
>     xs' <- permute ((take i xs) ++ (drop (i + 1) xs))
>     return ((xs!!i) : xs')

> prop_permuteStgs = do
>     n <- choose (2, 10)
>     collect n $ forAll (game2xN n) $ \g -> do
>         g' <- permuteStg g
>         return (snd (solution g) == snd (solution g'))

* The solution to a 2 x _n_ is the same as that of the 2 x 2 game returned
  with it.

> prop_reducedSame = do
>     n <- choose (2, 10)
>     collect n $ forAll (game2xN n) $ checkGame
>     where
>         checkGame g = sln == sln'
>             where
>                 (g', sln)  = solution g
>                 (_,  sln') = solution g'

* A game with a mixed meta-strategy does not have a saddlepoint.

> isPure (Pure _ _ _)  = True
> isPure _             = False

> hasSaddlePoint g = hasSP
>     where (hasSP, _, _, _)  = saddlePoint g

> prop_mixedNoSaddle = do
>     n <- choose (2, 10)
>     collect n $ forAll (game2xN n) $ checkGame
>     where
>         checkGame g = isPure sln || not (hasSaddlePoint g)
>             where sln = snd (solution g)

* The value of a 2 x 2 game is the additive inverse of the value
  of the transposed game, i.e. with players 1 and 2 switched.

Transposing a 2 x 2 game means changing player 2's columns into rows,
and multiplying all payoffs by -1, because the convention is that a
negative payoff benefits player 1, who has just become player 2.

> transpose_2x2 g
>     | not (is2x2 g) = error "transpose_2x2 applied to non-2x2 game"
>     | otherwise = g'
>     where
>         g' = g {
>               p1Name     = p2Name g
>             , p2Name     = p1Name g
>             , p1Stg1Name = (p2StgNames g) !! (i - 1)
>             , p1Stg2Name = (p2StgNames g) !! (j - 1)
>             , p2StgNames = [p1Stg1Name g, p1Stg2Name g]
>             , payoffs    = [(1,[(-a),(-b)]), (2,[(-c),(-d)])]
>             }
>         [(i,[a,c]), (j,[b,d])] = payoffs g

We need the value of a game but otherwise don't need the solution.

> value g = case snd (solution g) of
>     Pure _ _ v      -> v
>     Mixed _ _ _ _ v -> v

> prop_transposedValue = forAll (game2xN 2) $ \g-> 
>     value g == (-1) * (value (transpose_2x2 g))

* The value of a game doesn't change when player 1's strategies are swapped.

> prop_p1StgsSwapped = do
>     n <- choose (2, 10)
>     collect n $ forAll (game2xN n) $ \g-> value g == value (swapP1Stgs g)
>     where
>         swapP1Stgs g = g { payoffs = [(i,[b,a]) | (i,[a,b]) <- payoffs g] }

The following properties are yet to be implemented:

* A solution to a game with a dominant strategy doesn't change when that
  strategy is eliminated.
* The value of a 2 x 2 game to player 1 is the same regardless which
  strategy player 2 uses, and vice versa.
* The value of a 2 x _n_ game to player 1 is the best among all 2 x 2 games
  created from pairs of player 2's strategies.

Until I learn how to integrate property-based testing into 
`cabal build`, the following will allow running each test once by entering
`quickCheckAll` at the GHCI prompt, although the output won't be meaningful
unless one of the properties is falsified.

> properties = [
>        prop_numOpposingStrategies
>      , prop_numStrategies
>      , prop_addDominant
>      , prop_permuteStgs
>      , prop_reducedSame
>      , prop_mixedNoSaddle
>      , prop_transposedValue
>      , prop_p1StgsSwapped
>      ]

> quickCheckAll = mapM_ quickCheck properties

**Examples**

To make it easier to experiment with the functions above, here I
include a few of the examples from the book. Enter `solution saddle2x2`
or `showSolution saddle2x2` at the GHCI prompt to solve the `saddle2x2` game.

A non-numbered 2 x 2 example with a saddlepoint:

> saddle2x2 = mkStdGame2xN [6, 5, 5, 4]

Example 2. The Hidden Object:

> hiddenObject = mkGame2xN "Blue" "Red"
>                          "Bomb carrier in less-favored position"
>                          "Bomb carrier in favored position"
>                          ["Attack on less-favored position",
>                           "Attack on favored position"]
>                          [ (1,[60, 100]), (2,[100, 80])]

Example 3. The Dacquiris:

> dacquiris = mkGame2xN "Alex" "Olaf"
>                       "One finger" "Two fingers"
>                       ["One finger", "Two fingers"]
>                       [ (1,[55, 10]), (2,[10, 110]) ]

A non-numbered 2 x 4 example with a saddle point:

> saddle2x4 = mkStdGame2xN [1,4,7,8,0,(-1),3,6]

A non-numbered 2 x 7 example with a mixed-strategy solution:

> mixed2x7 = mkStdGame2xN [(-6),7,(-1),(-2),1,6,4,3,7,(-2),4,(-5),3,7]

Lastly, here's the collection of the examples. Entering `test` at
the GHCI prompt will solve all of them.

> examples = [
>       saddle2x2
>     , hiddenObject
>     , dacquiris
>     , saddle2x4
>     , mixed2x7
>     ]

> test = mapM_ showSolution examples
