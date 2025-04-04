module FSMMaker.StrategyBas 
   ( Strat 
   , Trail (Succeed, Fail, R, Lbl)
   , (.*.)
   , (<|>)
   , manyS
   , many1S
   , manyN
   , many1N
   , repeatS
   , repeat1S
   , repeatN
   , repeat1N
   , notS
   , try
   , option
   , (|>)
   , testStrat
   , applyAll
   , applyOnce) where
import FSMMaker.Trails 
import FSMMaker.Rules 
   ( Rule, rule, isBuggy, isMinor, ruleName
   , applyRule, notR, defaultRule)
import Utils.Utils (iterN)

type Strat a = Trail (Rule a) 

applyAll :: Strat a -> a -> [a]
applyAll s a = 
   [c | (r, s') <- firsts s
      , b <- applyRule r a, c <- applyAll s' b] ++
         if empty s then [a] else []

applyOnce :: Strat a -> a -> [(a, Strat a)]
applyOnce s a = 
   [(a', s') | (r, s') <- firsts s
             , a' <- applyRule r a] ++
         if empty s then [(a, Succeed)] else []

collapse :: Bool -> Bool -> String -> Strat a -> Rule a 
collapse b m n s = 
   defaultRule 
      { isBuggy = b
      , isMinor = m
      , ruleName = n
      , applyRule = applyAll s} 

notS :: Strat a -> Strat a
notS s = R $ notR $ collapse False True (show s) s

try :: Strat a -> Strat a
try s = s <|> notS s

option :: Strat a -> Strat a  
option s = s <|> Succeed 

manyS :: Strat a -> Strat a
manyS  = Many  

many1S :: Strat a -> Strat a
many1S s = s .*. Many s 

manyN :: Int -> Strat a -> Strat a
manyN n s = iterN n (\x -> Succeed <|> (s .*. x)) Succeed

many1N :: Int -> Strat a -> Strat a
many1N n s = s .*. manyN (n-1) s

repeatS :: Strat a -> Strat a
repeatS s = manyS s .*. (notS s)

repeat1S :: Strat a -> Strat a
repeat1S s = many1S s .*. (notS s)

repeatN :: Int -> Strat a -> Strat a
repeatN n s = iterN n (\x -> (try s) .*. x) Succeed

repeat1N :: Int -> Strat a -> Strat a
repeat1N n s = s .*. repeatN (n-1) s

(|>) :: Strat a -> Strat a -> Strat a
s |> t = s <|> (notS s) .*. t
infixl 5 |>

testStrat :: Strat a -> Strat a 
testStrat s = R $ rule "test" f 
   where f a = if null $ applyAll s a then [] else [a]