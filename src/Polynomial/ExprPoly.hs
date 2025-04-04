{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveFoldable #-}
module Polynomial.ExprPoly 
   ( ExprPoly(..)
   , var
   , constant
   , contextP  
   , transformP
   , toSingle
   , fromSingle
   , simplify
   , monic
   , getCoeff
   , leadCoeff
   , degree
   , (.-.)
   , (.^.)
   , terms2list
   , factors2list
   , mapTerms
   , nrOfTerms
   , nrOfFactors
   , toMulti
   , fromMulti
   , keepFactors
   , termForm
   , rebracket
   , reduceIDs
   , PolyEq(..)
   , solveDeg1
   , solveDeg2
   , mapEq
   , lhs
   , rhs
   , toZero
   , OrList (..)
   , mapOr
   , mapToOr
   , list2or
   , or2list
   , nrOfEqs
   , solveDegLT2) where
import Utils.Utils (getFix)
import GHC.Generics (Generic)
import Data.Data
import Data.Generics.Uniplate.Direct
import Polynomial.SinglePoly (SinglePoly)
import qualified Polynomial.SinglePoly as SP
import Polynomial.MultiPoly (MultiPoly)
import qualified Polynomial.MultiPoly as MP
import qualified Data.Map as Map (toList)
import Data.Set (Set)
import qualified Data.Set as Set 
import SymbolRoot.ComplexRoot (ComRoot, sqRoot)
import qualified SymbolRoot.ComplexRoot as CR (simplify)

data ExprPoly x a = ExprPoly x a :+: ExprPoly x a
                  | ExprPoly x a :*: ExprPoly x a 
                  | ExprPoly x a :^: Int
                  | N a
                  | Var x
   deriving (Eq, Ord, Data, Typeable, Generic, Foldable)

instance Functor (ExprPoly x) where 
   fmap g p = case p of 
      a :+: b -> fmap g a :+: fmap g b
      a :*: b -> fmap g a :*: fmap g b
      a :^: n -> (fmap g a) :^: n
      N a     -> N (g a) 
      Var x   -> Var x

var :: x -> ExprPoly x a
var = Var 

constant :: a -> ExprPoly x a 
constant = N

instance (Show x, Show a, Eq a, Num a) => Show (ExprPoly x a) where 
   show p = 
      let br s = case s of _:+:_ -> "(" ++ show s ++ ")"
                           _ -> show s
          brf s = case s of _:+:_ -> "(" ++ show s ++ ")"
                            _:*:_ -> "(" ++ show s ++ ")"
                            _ -> show s
      in case p of
            a :+: ((N (-1) :*: b) :+: d) -> 
               show a ++ " - " ++ br b ++ " + " ++ show d  
            a :+: (N (-1) :*: b) -> show a ++ " - " ++ br b    
            N(-1) :*: a -> "-" ++ br a
            a :+: b -> show a ++ " + " ++ show b
            a :*: b -> br a ++ " * " ++ br b
            a :^: n -> brf a ++ "^" ++ show n
            N a -> show a 
            Var x -> show x

instance Uniplate (ExprPoly x a) where
   uniplate (Var x)       = plate Var |- x
   uniplate (N a)      = plate N |- a  
   uniplate (p :^: n)  = plate (:^:) |* p |- n
   uniplate (a :+: b)  = plate (:+:) |* a |* b
   uniplate (a :*: b)  = plate (:*:) |* a |* b

contextP :: ExprPoly x a  -> 
   [(ExprPoly x a, ExprPoly x a  -> ExprPoly x a )]
contextP = contexts 

transformP :: 
   (ExprPoly x a -> ExprPoly x a) 
      -> ExprPoly x a -> ExprPoly x a
transformP = transform

toSingle :: (Ord x, Num a, Ord a) => ExprPoly x a -> SinglePoly x a
toSingle p = case p of 
   Var x -> SP.var x
   N a -> SP.constant a
   a :^: k -> toSingle a ^ k
   a :+: b -> toSingle a + toSingle b
   a :*: b -> toSingle a * toSingle b

fromSingle :: (Num a, Ord a) => SinglePoly x a -> ExprPoly x a
fromSingle = foldr1 (:+:) . map f . SP.getTerms 
   where f (mv,a) = case mv of Nothing -> N a
                               Just (x, k) 
                                  | k == 1 && a == 1 -> Var x 
                                  | k == 1 && a /= 1 -> N a :*: Var x 
                                  | a == 1 -> Var x :^: k 
                                  | otherwise -> N a :*: (Var x :^: k)

simplify :: (Ord x, Num a, Ord a) 
   => ExprPoly x a -> ExprPoly x a
simplify = fromSingle . toSingle

monic :: (Fractional a, Ord a, Ord x) => ExprPoly x a -> ExprPoly x a
monic = fromSingle . SP.monic . toSingle

getCoeff :: (Ord x, Num a, Ord a) => x -> Int -> ExprPoly x a -> a
getCoeff x n = SP.getCoeff x n . toSingle

leadCoeff :: (Ord x, Num a, Ord a) => ExprPoly x a -> a
leadCoeff = SP.leadCoeff . toSingle

degree :: (Ord x, Num a, Ord a) => ExprPoly x a -> Int
degree = SP.degree . toSingle

sign :: (Ord x, Num a, Ord a) 
   => ExprPoly x a -> ExprPoly x a
sign = fromSingle . signum . toSingle 

instance (Ord x, Num a, Ord a) => Num (ExprPoly x a) where 
   (a :+: b) + c = a + (b + c)
   a + b = a :+: b
   (a :*: b) * c = a * (b * c)
   a * b = a :*: b
   negate a = N (-1) * a
   signum = sign
   abs a = (sign a) * a
   fromInteger = N . fromInteger

(.^.) :: ExprPoly x a -> Int -> ExprPoly x a
(.^.) = (:^:) 
infixr 8 .^.

workOutMins :: (Ord x, Num a, Ord a) => ExprPoly x a -> ExprPoly x a
workOutMins p = case p of 
   N (-1) :*: a -> mapTerms (N (-1) *) a
   a -> a

(.-.) :: (Ord x, Num a, Ord a) => ExprPoly x a -> ExprPoly x a -> ExprPoly x a
a .-. b = a + workOutMins (negate b)
infix 5 .-.

rebracket :: (Eq x, Num a, Eq a) => ExprPoly x a -> ExprPoly x a
rebracket = getFix $ transform f
   where 
      f p = case p of 
         (a :+: b) :+: c -> a :+: (b :+: c)
         (a :*: b) :*: c -> a :*: (b :*: c)
         a -> a

reduceIDs :: (Ord x, Num a, Ord a) => ExprPoly x a -> ExprPoly x a
reduceIDs = getFix $ transform $ rebracket . f
   where 
      f p = case p of 
         1 :*: a -> a 
         a :*: 1 -> a 
         0 :+: a -> a
         a :+: 0 -> a  
         0 :*: _ -> 0
         _ :*: 0 -> 0
         a -> a

terms2list :: ExprPoly x a -> [ExprPoly x a]
terms2list p = case p of 
   a :+: b -> a : terms2list b
   a -> [a]

factors2list :: ExprPoly x a -> [ExprPoly x a]
factors2list p = case p of 
   a :*: b -> a : factors2list b
   a -> [a]

mapTerms :: Num b =>
   (ExprPoly x a -> b) 
      -> ExprPoly x a -> b
mapTerms f = foldr1 (+) . map f . terms2list

nrOfTerms, nrOfFactors :: ExprPoly x a -> Int
nrOfTerms = length . terms2list
nrOfFactors = length . factors2list

toMulti :: 
   (Ord x, Num a, Ord a) => 
      ExprPoly x a -> MultiPoly (ExprPoly x a) a
toMulti = mapTerms f 
   where 
      f q = case q of 
         N a :*: b -> (MP.constant a) * (f b)
         N a -> MP.constant a
         a :^: n -> (f a) ^ n
         a :*: b -> f a * f b 
         a -> MP.var a

fromMulti :: 
   (Ord x, Num a, Ord a) => 
      MultiPoly (ExprPoly x a) a -> ExprPoly x a 
fromMulti p = foldr1 (+) $ do 
   (ps, a) <- (MP.getTerms p)
   if (null ps) then return (N a) else do 
      let term = foldr1 (*) $ map f $ Map.toList ps
      if a == 1 then return term else return (N a * term)
   where f (x, n) = if n == 1 then x else x :^: n

keepFactors :: (Ord x, Num a, Ord a) =>  ExprPoly x a -> ExprPoly x a
keepFactors = getFix $ transform $ fromMulti . toMulti 

termForm :: (Ord x, Num a, Ord a) =>  ExprPoly x a -> Bool
termForm p = keepFactors (simplify $ keepFactors p) == keepFactors p 

data PolyEq x a = ExprPoly x a :=: ExprPoly x a 
   deriving (Eq, Ord)
infix 4 :=:

instance (Show x, Show a, Eq a, Num a) => Show (PolyEq x a) where 
   show (a :=: b) = show a ++ " = " ++ show b 

solveDeg1 :: PolyEq String ComRoot -> [PolyEq String ComRoot]
solveDeg1 p = if degree q == 1 
              then [var "x" :=: N (CR.simplify $ -b/a)]
              else []
   where q = toZero p
         a = getCoeff "x" 1 q
         b = getCoeff "x" 0 q

solveDeg2 :: PolyEq String ComRoot -> [PolyEq String ComRoot]
solveDeg2 p = if degree q == 2 
              then [ var "x" :=: N (CR.simplify $ (-b - sqRoot d)/(2*a))
                   , var "x" :=: N (CR.simplify $ (-b + sqRoot d)/(2*a))]
              else []
   where q = toZero p
         a = getCoeff "x" 2 q
         b = getCoeff "x" 1 q
         c = getCoeff "x" 0 q
         d = b^2-4*a*c

lhs :: PolyEq x a -> ExprPoly x a
lhs (a :=: _) = a 

rhs :: PolyEq x a -> ExprPoly x a
rhs (_ :=: b) = b

mapEq :: 
   (ExprPoly x a -> ExprPoly x a)
      -> PolyEq x a -> PolyEq x a
mapEq f (a :=: b) = f a :=: f b

toZero :: (Ord x, Num a, Ord a) => PolyEq x a -> ExprPoly x a
toZero (a :=: b) = a .-. b 

data OrList x a = OrList {or2set :: Set (PolyEq x a)}
   deriving (Eq, Ord)

mapOr :: Ord b => 
   (PolyEq x a -> b) -> OrList x a -> Set b
mapOr f = Set.map f . or2set 

mapToOr :: (Ord b, Ord y) => 
   (PolyEq x a -> PolyEq y b) -> OrList x a -> OrList y b
mapToOr f = OrList . Set.map f . or2set  

list2or :: (Ord a, Ord x) => [PolyEq x a] -> OrList x a 
list2or = OrList . Set.fromList

or2list :: OrList x a -> [PolyEq x a]
or2list = Set.toList . or2set

instance (Show x, Show a, Eq a, Num a) => Show (OrList x a) where 
   show = show . or2list

nrOfEqs :: OrList x a -> Int
nrOfEqs = Set.size . or2set

solveDegLT2 :: OrList String ComRoot -> [OrList String ComRoot]
solveDegLT2 p = case traverse f $ or2list p of 
                [] -> []
                p' -> [list2or $ concat p']
   where f q = case solveDeg1 q of 
            [] -> solveDeg2 q
            s  -> s