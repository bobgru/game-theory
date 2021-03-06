-- Solving 2 x N Games
--
-- from The Compleat Strategyst, by J.D. Williams.

module Game2xN ( Game2xN
               , p1Name, p2Name
               , p1Stg1Name, p1Stg2Name, p2StgNames
               , payoffs
               , Strategy, Player, StrategyId, Payoff
               , mkGame2xN, mkStdGame2xN
               , Solution(..)
               , solution, showSolution, fmtSolution
               , is2x2, saddlePoint
               )
where
import Data.List(maximumBy,minimumBy,sortBy)
import Control.Arrow(second)

-- Introduction
-- 
-- A game is a formalized conflict in which there are two players
-- with opposing interests. Each player has some number of strategies
-- according to which to engage the opponent. Taking all combinations
-- of each player's strategies against the other's gives a matrix where
-- an entry represents the relative payoff. By convention a positive
-- payoff means player 1 wins; negative, player 2. All of the meaning
-- of playing the game is encoded into the payoff matrix. Being able
-- to do that is a considerable skill, but here we just assume it's
-- been done. Another assumption we make is that both players know
-- all the payoffs.
-- 
-- The early chapters of the book give recipes for solving 2 x 2, 2 x n,
-- and n x 2 games, which are implemented in this module.
-- 
-- As some of the examples add colorful interpretations to the
-- players and strategies, we'll make room for them in our data type.
-- However, most of the examples use a standard setup of Blue as player 1
-- and Red as player 2, with strategies numbered from 1.
-- Blue's strategies are rows of the matrix, whereas Red's are
-- columns.
-- 
-- Solving a game means determining the combination of strategies
-- each player should use to maximize the average payoff over
-- time, otherwise known as the value. It turns out that some
-- solutions indicate a pure strategy in which one is always best,
-- and some have a mixed meta-strategy in which each pure strategy
-- is used at random but according to a specified distribution.
-- 
-- Solving 2 x 2 games is simplest. For 2 x n and n x 2 games, solving means
-- reducing the matrix to 2 x 2, or possibly a collection of 2 x 2's, and
-- comparing them to find the best value. For simplicity we can represent
-- a game as 2 x n, because an n x 2 game can be transposed into a 2 x n,
-- and 2 x 2 is a special case within 2 x n.
-- 
-- Thus we have the requirements for our main data type.
-- 
-- The Game2xN Data Type
-- 
-- Player 1 has two strategies. Player 2 has n. The payoff matrix
-- is represented as a list of columns, i.e. player 2's strategies,
-- tagged with strategy number. Player 1's strategies are implicit
-- as the collection of all first or of all second entries of the columns.
-- Each player has a name, as do the strategies. Player 2's strategy
-- names are in a list which must agree with the number of columns in
-- the payoff matrix, which really means it must be long enough to
-- look up any strategy number which might appear in the payoff matrix.

data Game2xN = G2N {
      p1Name     :: String
    , p2Name     :: String
    , p1Stg1Name :: String
    , p1Stg2Name :: String
    , p2StgNames :: [String]
    , payoffs    :: [Strategy]
} deriving (Eq, Show)

type Strategy   = (StrategyId, [Payoff])
type Player     = Int
type StrategyId = Int
type Payoff     = Integer

-- We'll need to create games, so define some functions to help.
-- The first one doesn't appear to help much, but lets us hide
-- the data constructor.

mkGame2xN :: String -> String -> String -> String
          -> [String] -> [Strategy] -> Game2xN
mkGame2xN = G2N

-- For games that consist only of a payoff matrix, it will be convenient
-- to have the other fields filled in automatically. The ps argument is
-- a list of Payoff, not Strategy, so we have to check that we can create
-- player 2's strategies from it.

mkStdGame2xN :: [Payoff] -> Game2xN
mkStdGame2xN ps
    | even n = G2N {
          p1Name = "Blue"
        , p2Name = "Red"
        , p1Stg1Name = "Blue 1"
        , p1Stg2Name = "Blue 2"
        , p2StgNames = ["Red " ++ show i | i <- [1..n2]]
        , payoffs =  [(i, ss) | (ss, i) <- zip (chunk 2 ps) [1..]]
        }
    | otherwise = error "2xN game must have even number of payoffs"
    where
        n  = length ps
        n2 = n `div` 2

chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- Given a game, we want to know the solution. A pure strategy is
-- represented by the strategy numbers for player 1 and player 2,
-- respectively, and the game value. A mixed meta-strategy supplies
-- the strategy numbers of the best 2 x 2 submatrix of payoffs, and
-- the percentage of time each strategy should be used, along with
-- the game value.

data Solution = Pure StrategyId StrategyId Float
              | Mixed {
                    p1SlnStg1 :: (StrategyId, Float)
                  , p1SlnStg2 :: (StrategyId, Float)
                  , p2SlnStg1 :: (StrategyId, Float)
                  , p2SlnStg2 :: (StrategyId, Float)
                  , slnValue  :: Float
              } deriving (Eq, Show)

-- Solution Plan A: Find a Saddlepoint
-- 
-- Finding a solution involves a succession of ad hoc checks.
-- 
-- First, if the game has a pure solution, the payoff matrix will
-- have a saddlepoint. Player 1 would like to pick a strategy to
-- maximize the payoff to her, but whichever strategy she chooses,
-- player 2 may play so as to minimize it. In matrix terms, regardless
-- the row player 1 selects, player 2 could, and would like to, take
-- the minimum value in it. Conversely, player 2 wants the smallest
-- payoff, ideally negative, but runs the risk of player 1 getting
-- the maximum from his column.
-- 
-- Player 1 can set a lower bound on her payoff, by using the strategy
-- with the largest minimum, the maxmin. Player 2 can likewise set an
-- upper bound on his losses by choosing the column with the smallest
-- maximum, the minmax.
-- 
-- If the maxmin and minmax happen to be the same number, the game has
-- a saddlepoint. Each player should always use the specified strategy,
-- and the intersecting payoff is the game's value.

saddlePoint :: Game2xN -> (Bool, Float, StrategyId, StrategyId)
saddlePoint g = (b, v, r, c)
    where
        (r, maxmin) = rowMaxOfMins g
        (c, minmax) = colMinOfMaxs g
        b = maxmin == minmax
        v = if b then fromIntegral maxmin else 0.0

rowMaxOfMins :: Game2xN -> (StrategyId, Payoff)
rowMaxOfMins g = maximumBy cmpStg mins
    where mins = map (second minimum) (rows g)

-- To guarantee a consistent order in the face of possibly redundant
-- strategies, consider first the strategy, then the strategy number.

cmpStg (i,x) (j,y) = compare (x,i) (y,j)

colMinOfMaxs :: Game2xN -> (StrategyId, Payoff)
colMinOfMaxs g = minimumBy cmpStg maxs
    where maxs = map (second maximum) (cols g)

rows g = zip [1..] ss
    where
        ps = payoffs g
        ss = [map (head . snd) ps, map (last . snd) ps]

cols = payoffs

-- Solution Plan B: Eliminate Dominance
-- 
-- If the 2 x n game does not have a saddlepoint, we move to the next
-- check. There may be strategies for player 2 which are obviously bad,
-- in the sense that every payoff is worse than for some other strategy.
-- We will call such a strategy dominant when its payoffs are higher
-- than the other's, and assume that an intelligent player 2 would
-- never use it.
-- 
-- As we have tagged player 2's strategies with their numbers, we can
-- freely rearrange them. By sorting in reverse order according to
-- the payoff values, which puts dominant strategies ahead of dominated
-- ones, we can check each in turn against those that follow, and
-- keep or discard as necessary.

x `dominates` y = and [a >= b | (a,b) <- zip x y]

purgeDominant :: [Strategy] -> [Strategy]
purgeDominant = purgeDominant' . sortBy (flip cmpStg)

purgeDominant' :: [Strategy] -> [Strategy]
purgeDominant' [] = []
purgeDominant' (s@(_,x):xs)
    | any ((x `dominates`) . snd) xs =     purgeDominant' xs
    | otherwise                      = s : purgeDominant' xs

-- If the payoff matrix reduces to 2 x 2 after eliminating dominant
-- strategies, we have a simple technique for solving it. We'll
-- check that frequently, so encapsulate in a function. Do the same
-- with an error reporting function for non-2 x 2 games given to
-- functions that expect 2 x 2's.

is2x2 :: Game2xN -> Bool
is2x2 g = length (payoffs g) == 2

errNon2x2 :: String -> a
errNon2x2 s = error (s ++ " applied to non-2x2 game")

-- We again check for a saddlepoint and are done if we find one.
-- If none, calculate the distribution according to which
-- to use each strategy and report as a mixed meta-strategy.

solution2x2 :: Game2xN -> Solution
solution2x2 g
    | not (is2x2 g) = errNon2x2 "solution2x2"
    | otherwise =
        case saddlePoint g of
            (True, v, r, c) -> Pure r c v
            otherwise       -> Mixed {
                                  p1SlnStg1 = (p11, pct11)
                                , p1SlnStg2 = (p12, pct12)
                                , p2SlnStg1 = (p21, pct21)
                                , p2SlnStg2 = (p22, pct22)
                                , slnValue  = mv
                                }
    where
        ((p11, pct11), (p12, pct12), mv) = mixedSln2x2 1 g
        ((p21, pct21), (p22, pct22), _)  = mixedSln2x2 2 g

-- The book takes several pages to explain the process of computing
-- odds and value for a mixed meta-strategy. Rather than do a
-- worse job at that, I submit the code.
-- 
-- The mixedSln2x2 function calculates the distribution and value
-- for the specified player. It assumes that there is no saddlepoint.

mixedSln2x2 :: Player -> Game2xN
            -> ((StrategyId, Float), (StrategyId, Float), Float)
mixedSln2x2 p g
    | not (is2x2 g) = errNon2x2 "mixedSln2x2"
    | otherwise     = fix ((i, o1pct), (j, o2pct), v)
    where
        odds@[o1,o2] = odds2x2 p g
        [i,j]  = if p == 1 then [1,2] else map fst (payoffs g)
        n      = sum (zipWith (*) odds (ps2x2 (opponent p) 1 g))
        d      = fromIntegral (sum odds)
        v      = fromIntegral  n / d
        o1pct  = fromIntegral o1 / d
        o2pct  = fromIntegral o2 / d
        fix r@((x,a),(y,b),v) = if x < y then r else ((y,b),(x,a),v)

-- See the book for why the odds are calculated as they are. As for how,
-- we produce two numbers corresponding to a player's strategies such that
-- either of them over their sum gives the percentage of time to use
-- that strategy.

odds2x2 :: Player -> Game2xN -> [Payoff]
odds2x2 p g
    | not (is2x2 g) = errNon2x2 "odds2x2"
    | p == 1        = [ abs (c-d), abs (a-b) ]
    | p == 2        = [ abs (b-d), abs (a-c) ]
    | otherwise     = error (errMsgP p)
    where [(_,[a,c]), (_,[b,d])] = payoffs g

-- In mixedSln2x2 we relied on a function to supply the payoffs for
-- a particular player and strategy, and another to produce the opponent
-- given a player.

ps2x2 :: Player -> StrategyId -> Game2xN -> [Payoff]
ps2x2 p s g
    | not (is2x2 g) = errNon2x2 "ps2x2"
    | otherwise =
        case (p,s) of
            (1,1) -> [a,b]
            (1,2) -> [c,d]
            (2,1) -> [a,c]
            (2,2) -> [b,d]
            otherwise -> error (errMsgPS p s)
    where [(_,[a,c]), (_,[b,d])] = payoffs g

opponent :: Player -> Player
opponent 1 = 2
opponent 2 = 1
opponent p = error (errMsgP p)

errMsgP :: Player -> String
errMsgP p = "Bad player number: " ++ show p

errMsgPS :: Player -> StrategyId -> String
errMsgPS p s = "Bad player " ++ show p ++ " or strategy " ++ show s

-- Solution Plan C: Compare Subgames
-- 
-- If the payoff matrix did not reduce to 2 x 2 after eliminating
-- dominant strategies for player 2, we fall back on the general
-- method for solving 2 x n games.
-- 
-- We try each pair of player 2's strategies against player 1's
-- to make a 2 x 2 game. We compare them all and report the minimum
-- value, as we are searching for player 2's optimum meta-strategy.

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs [x]    = []
pairs (x:xs) = [(x,y)|y<-xs] ++ pairs xs

-- We only need to pair up the strategy numbers, as we can look up
-- the strategy from the game.

spairs :: Game2xN -> [(StrategyId,StrategyId)]
spairs g = pairs ids
    where ids = map fst (cols g)

-- Given a pair of strategy numbers for player 2, produce a new game
-- with just those. The payoff matrix must contain the given pair of
-- strategies. We leave the strategy name list alone so that whichever
-- pair is selected, its indices can be used to find the correct names.

g22From2N :: Game2xN -> (StrategyId,StrategyId) -> Game2xN
g22From2N g (i, j)
    | (i `elem` ids) && (j `elem` ids) = g {payoffs = [p2Stg1, p2Stg2]}
    | otherwise = error ("Strategies " ++ show i ++ " and " ++ show j ++
                         " not found in 2xN game")
    where
        ids = map fst (cols g)
        p2Stg1 = head (filter ((==i) . fst) (cols g))
        p2Stg2 = head (filter ((==j) . fst) (cols g))

-- We can now take the smaller 2 x 2 games involving each pair of
-- player 2's strategies, solve them, and report the most favorable
-- as our overall solution. For convenience of further analysis,
-- we also report the 2 x 2 game which won, although the solution
-- contains enough information to reconstruct it.

solution2xN :: Game2xN -> (Game2xN, Solution)
solution2xN g = minimumBy cmpSln g_slns
    where
        g22s    = map (g22From2N g) (spairs g)
        g_slns  = map g_sln g22s
        g_sln g = (g, solution2x2 g)

-- Comparing solutions really means comparing game values, so extract
-- those.

cmpSln :: (Game2xN, Solution) -> (Game2xN, Solution) -> Ordering
cmpSln (_,Pure _ _ v1)      (_,Pure _ _ v2)      = compare v1 v2
cmpSln (_,Pure _ _ v1)      (_,Mixed _ _ _ _ v2) = compare v1 v2
cmpSln (_,Mixed _ _ _ _ v1) (_,Pure _ _ v2)      = compare v1 v2
cmpSln (_,Mixed _ _ _ _ v1) (_,Mixed _ _ _ _ v2) = compare v1 v2

-- Solution, the Overall Plan
-- 
-- With all the support in place, here is the organizing function
-- to solve a 2 x n game. The solution is reported along with the
-- possibly modified game used to find it.

solution :: Game2xN -> (Game2xN, Solution)
solution g
    | hasSaddlePoint  = (g,   Pure r c v)
    | is2x2 g'        = (g',  solution2x2 g')
    | otherwise       =       solution2xN g'
    where
        (hasSaddlePoint, v, r, c) = saddlePoint g
        g' = g {payoffs = purgeDominant (payoffs g)}

-- We included the player and strategy names so we can translate the
-- solution into a more readable form.

showSolution :: Game2xN -> IO ()
showSolution g = putStrLn (fmtSolution (solution g))

fmtSolution :: (Game2xN, Solution) -> String
fmtSolution (g, Pure p1 p2 v) =
    "Pure strategy: Value = " ++ show v ++ "\n" ++
    "  " ++ fmtPlayer 1 p1 g ++ "\n" ++
    "  " ++ fmtPlayer 2 p2 g
    where
        fmtPlayer p s g =
            pName p g ++ ": " ++ sName p s g

fmtSolution (g, Mixed p11 p12 p21 p22 v) =
    "Mixed strategy: Value = " ++  show v ++ "\n" ++
    fmtPlayer 1 g p11 p12 ++ "\n" ++
    fmtPlayer 2 g p21 p22
    where
        fmtPlayer p g s1 s2 =
            pName p g ++ ":\n  " ++
            sName p (fst s1) g ++
            " (" ++ fmtPct (snd s1) ++ "%)\n  " ++
            sName p (fst s2) g ++
            " (" ++ fmtPct (snd s2) ++ "%)"

-- The fmtSolution function relies on helpers to determine player
-- and strategy names and to format percentages.

pName :: Player -> Game2xN -> String
pName 1 = p1Name
pName 2 = p2Name

sName :: Player -> StrategyId -> Game2xN -> String
sName 1 1 g = p1Stg1Name g
sName 1 2 g = p1Stg2Name g
sName p s g
    | p == 2 && 0 < s && s <= length (p2StgNames g) = p2StgNames g !! (s-1)
    | otherwise = error (errMsgPS p s)

fmtPct :: Float -> String
fmtPct p = show (round (p * 100))
