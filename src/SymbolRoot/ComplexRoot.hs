module SymbolRoot.ComplexRoot 
   ( ComRoot
   , highRoot
   , sqRoot
   , simplify
   , eval
   , sqrtSim
   , isInt
   , isRat
   , im
   , imp 
   , re
   , rep) where 
import SymbolRoot.ExprRoot (ExprRoot)
import qualified SymbolRoot.ExprRoot as ER
import Utils.ComplexNum

type ComRoot = Complex ExprRoot

highRoot :: Integer -> ComRoot -> ComRoot
highRoot k r  
   | imp r /= 0 = error "only defined for real numbers"
   | signum a >= 0 = re $ ER.highRoot k a
   | signum a <  0 && even k = im $ ER.highRoot k $ abs a
   | otherwise = - (re $ ER.highRoot k $ abs a)  
   where a = rep r

sqRoot :: ComRoot -> ComRoot
sqRoot = highRoot 2 

simplify :: ComRoot -> ComRoot
simplify = fmap ER.simplify

eval :: ComRoot -> Complex Double
eval = fmap ER.eval

sqrtSim :: ComRoot -> ComRoot
sqrtSim = simplify . sqRoot

isInt :: ComRoot -> Bool
isInt r = ER.isInt (rep r) && imp r == 0 

isRat :: ComRoot -> Bool
isRat r = ER.isRat (rep r) && imp r == 0