module Polynomial.PolyBuggys where 
import Polynomial.ExprPoly
import FSMMaker.Rules 
import Regulation.Focus 
import Polynomial.PolyLift 
import qualified SymbolRoot.ExprRoot as ER (constant, eval)
import SymbolRoot.ComplexRoot 
import Utils.Utils(roundDown, roundUp)

negateTerm :: (Num a, Ord a) => Rule (Focus (PEX a))
negateTerm = lift2focus $ rPEX $ buggyRule "negate a term" f 
   where f p = [-p]

negateSolution :: (Num a, Ord a) => Rule (Focus (PEX a))
negateSolution = lift2focus $ rPEQ $ buggyRule "negate solution" f 
   where f p = case p of 
               Var "x" :=: N a | a /= 0 -> [Var "x" :=: N (-a)]
               _ -> []

forgetEquation :: Ord a => Rule (Focus (PQS a)) 
forgetEquation = lift2focus $ rL2PQS $ buggyRule "forget an equation" f
   where 
      f p = case p of 
         [x, y] -> [[x], [y]] 
         _ -> []

workoutSquareB1 :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquareB1 = lift2focus $ rPEX $ buggyRule "square of sum is sum of squares" f 
   where 
      f p = case p of 
          (a:+:b):^:2 -> [ a.^.2 + b.^.2]
          _ -> []

workoutSquareB2 :: (Num a, Ord a) => Rule (Focus (PEX a))
workoutSquareB2 = lift2focus $ rPEX $ buggyRule "square of sum is sum of squares" f 
   where 
      f p = case p of 
          (a :*: b :+: c):^:2 -> [ a * b.^.2 + c.^.2]
          (c :+: (a :*: b)):^:2 -> [ c.^.2 + a * b.^.2]
          _ -> []

divideReverse :: (Fractional a, Ord a) => Rule (Focus (PEQ a))
divideReverse = 
   lift2focus $ rPEQ $ buggyRule "reversely divide by a in a*B=c" f
   where 
      f p = case p of 
         N a :*: b :=: N c | c /= 0 -> [b :=: N (a/c)]
         _ -> []

divideSubtract :: (Num a, Ord a) => Rule (Focus (PEQ a))
divideSubtract = 
   lift2focus $ rPEQ $ buggyRule "subtract a in a*B=c" f
   where 
      f p = case p of 
         N a :*: b :=: N c -> [b :=: N (c - a)]
         _ -> []

divideForget :: (Num a, Eq a) => Rule (Focus (PEQ a))
divideForget = 
   lift2focus $ rPEQ $ buggyRule "forget a in a*B=c" f
   where 
      f p = case p of 
         N a :*: b :=: N c | a /= 1 -> [b :=: N c]
         _ -> []

nillProductB :: (Num a, Ord a) => Rule (Focus (PQS a))
nillProductB = 
   lift2focus $ rL2PQS $ buggyRule "A*B=C => A=C or B=C" f
   where 
      f p = case p of 
         [a :*: b :=: N c] | c /= 0 -> [[a :=: N c, b :=: N c]]
         _ -> []

approximateRoot :: Int -> Rule (Focus (PEX ComRoot))
approximateRoot n = lift2focus $ rPEX $ buggyRule "approximate a root" f
   where 
      f p = case p of 
         N r | isRat r -> []
             | imp r /= 0 -> []
             | otherwise -> 
                [ N $ re $ ER.constant $ roundDown n $ ER.eval $ rep r
                , N $ re $ ER.constant $ roundUp n $ ER.eval $ rep r]  
         _ -> []

roundRootsUp :: Int -> Rule (Focus(PQS ComRoot))
roundRootsUp n = lift2focus $ rPQS $ buggyRule "approximate roots" f
   where f p = if r p == p then [] else [r p]
         r = mapToOr (mapEq (fmap g)) 
         g x = re $ ER.constant $ roundUp n $ ER.eval $ rep x

roundRootsDown :: Int -> Rule (Focus(PQS ComRoot))
roundRootsDown n = lift2focus $ rPQS $ buggyRule "approximate roots" f
   where f p = if r p == p then [] else [r p]
         r = mapToOr (mapEq (fmap g)) 
         g x = re $ ER.constant $ roundDown n $ ER.eval $ rep x

linSquareForgetRoot :: Rule (Focus (PEQ ComRoot))
linSquareForgetRoot = 
   lift2focus $ rL2PQS $ buggyRule "A^2 = b -> A = b, A = - b" f 
   where 
      f p = case p of 
         [N a :*: (x:^:2) :=: N b] -> 
            [[N a * x :=: N (-b), N a * x :=: N b]]
         [x:^:2 :=: N b] -> [[x :=: N (-b), x :=: N b]]
         _ -> []

linSquareForgetRootConst :: Rule (Focus (PEQ ComRoot))
linSquareForgetRootConst = 
   lift2focus $ rL2PQS $ buggyRule "aA^2 = sqrt b -> aA = b, aA = - sqrt b" f 
   where 
      f p = let r = sqrtSim in case p of 
         [N a :*: (x:^:2) :=: N b] -> 
            [[N a * x :=: N (-r b), N a * x :=: N (r b)]]
         _ -> []

abcForgetRoot :: Rule (Focus (PQS ComRoot))
abcForgetRoot = lift2focus $ rL2PQS $ buggyRule "forget root in the abcFormula" f 
   where 
      f p = case map toZero p of 
         [q]  -> 
            [[ var "x" :=: N ((-b - d)/(2*a))
             , var "x" :=: N ((-b + d)/(2*a))]]
            where a = getCoeff "x" 2 q
                  b = getCoeff "x" 1 q
                  c = getCoeff "x" 0 q
                  d = b^2-4*a*c
         _ -> []

abcDivideBy_a :: Rule (Focus (PQS ComRoot))
abcDivideBy_a = lift2focus $ rL2PQS $ buggyRule "forget root in the abcFormula" f 
   where 
      f p = case map toZero p of 
         [q]  -> 
            [[ var "x" :=: N ((-b - sqrtSim d)/a)
             , var "x" :=: N ((-b + sqrtSim d)/a)]]
            where a = getCoeff "x" 2 q
                  b = getCoeff "x" 1 q
                  c = getCoeff "x" 0 q
                  d = b^2-4*a*c
         _ -> []