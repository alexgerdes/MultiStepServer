module Polynomial.PolyRelations where 
import Diagnosis.RelationTree hiding (embed)
import Polynomial.ExprPoly 
import SymbolRoot.ComplexRoot hiding (simplify)
import qualified SymbolRoot.ComplexRoot as CR (simplify)
import Data.List (sortBy)
import Polynomial.PolyStrat

type OrListC = OrList String ComRoot
type PolyEqC = PolyEq String ComRoot
type ExprPolyC = ExprPoly String ComRoot 

(.=>) :: Bool -> Bool -> Bool
p .=> q = not p || p && q 

normal :: PolyEqC -> PolyEqC 
normal p = 
   (fmap CR.simplify $ keepFactors $ mapTerms (a *) $ toZero p) :=: 0
      where a = case leadCoeff (toZero p) of 
                0  -> 1
                a' -> N (CR.simplify $ 1/a')

normalOr :: OrListC -> OrListC
normalOr = mapToOr normal

distributed :: OrListC -> OrListC -> Bool
distributed p q = 
   ((normalOr q) `elem`) . map normalOr . distSet $ p

matchEqs :: OrListC -> OrListC -> [(PolyEqC, PolyEqC)]
matchEqs p q = zip (sortBy c $ or2list p) (sortBy c $ or2list q)
   where c x y = compare (normal x) (normal y) 

expectedNrEqs :: OrListC -> RelationTree OrListC
expectedNrEqs b = test (((nrOfEqs b) ==) . nrOfEqs) "expected number of equations"

expectedNormal :: OrListC -> RelationTree OrListC
expectedNormal b = test f "expected normal form"
   where f a = normalOr a == normalOr b

expectedNrOfTerms :: OrListC -> RelationTree OrListC
expectedNrOfTerms b = test f "expected number of terms"
   where 
      f a = all c (matchEqs a b)
      c (a', b') = nrt a' >= nrt b'
      nrt = nrOfTerms . reduceIDs . toZero 

obtainedByDist :: OrListC -> OrListC -> RelationTree OrListC
obtainedByDist a b = binary (distributed a b) "obtained by distribution" 

expectedZero :: OrListC -> RelationTree OrListC
expectedZero b = test f "derived to zero as expected"
   where 
      f a = all c (matchEqs a b)
      c (a', b') = z a' .=> z b' && 
                   lq a' .=> (z b'.=> z a')
      z (l:=:r) = l == 0 || r == 0
      lq x = case normal x of 
             N _ :+: (N _ :*: ((_ :+: _):^:2)) :=: 0 -> True 
             N _ :*: ((_ :+: _):^:2) :=: 0 -> True
             N _ :+: ((_ :+: _):^:2) :=: 0 -> True
             (_ :+: _):^:2 :=: 0 -> True 
             _ -> False

expectedSquares :: OrListC -> RelationTree OrListC
expectedSquares b = test f "has squares as expected"
   where 
      f a = all c (matchEqs a b)
      c (a', b') = s a' == s b'
      s x = null [ p |          x' <- terms2list (toZero x)
                     , (p:+:_):^:2 <- factors2list x']

expectedMonic :: OrListC -> RelationTree OrListC
expectedMonic b = test f "is monic as expected"
   where 
      f a = all c (matchEqs a b)
      c (a', b') = m a' == m b'
      m x = leadCoeff (toZero x) == 1 

polyTree :: OrListC -> OrListC -> RelationTree OrListC
polyTree a b = expectedNrEqs b 
         .>> (expectedNormal b |> obtainedByDist a b .>> stop) 
         .>> expectedNrOfTerms b
         .>> expectedZero b
         .>> expectedSquares b
         .>> expectedMonic b