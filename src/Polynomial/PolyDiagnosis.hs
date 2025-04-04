module Polynomial.PolyDiagnosis where
import Regulation.Search 
import Regulation.Tracer hiding (embed)
import Regulation.Focus
import Diagnosis.RelationTree (apply, TreeTracer, unSat, sat)
import Diagnosis.MBTdiagnosis (countBuggy)
import Polynomial.ExprPoly
import Polynomial.PolyLift (PQS (..))
import Polynomial.PolyStrat
import Polynomial.BuggyStrat
import Polynomial.PolyRelations
import FSMMaker.FSM 
import Utils.Utils (setNub)
import Communication.ParseExpr (parse2or, replace)
import SymbolRoot.ComplexRoot
import Data.Either (isRight)
import Data.List (intersperse)

polyDiagnosis
  :: OrListC
     -> OrListC
     -> Either [[String]] (TreeTracer OrListC)
polyDiagnosis a b = 
   if s a == s b
   then Right $ apply (polyTree a b) 
              $ map getElement pts
   else Left  $ nubSubList
              -- $ setNub $ map setNub  
              $ concatMap trace 
              $ filter ((== (s b)) . (:[]) .  getElement) 
             pts
   where s = solveDegLT2 
         getElement = pqs . top . element
         pts = map fst $
               (if s a == s b 
                  then derivations (fsm $ fmap neutral2tracer polyStrat)  
                  else applyAll (fsm $ fmap (countBuggy 2 . buggy2tracer) buggyStrat)) $ 
               embed $ PQS a

--tester a = 
--   map (top . element .fst) $ 
--   applyAll (fsm $ fmap neutral2tracer linSquareStratB) $ 
--   embed $ PQS a

stringDiagnosis :: (String, String) -> (String, Integer)
stringDiagnosis (t, a) = 
   case polyDiagnosis <$> (parse2or t) <*> (parse2or a) of
      Left msg -> ("parsing error:<br>" ++ (replace "\n" "<br>" msg), 3) 
      Right d  -> case d of 
         Left mbt -> ("diagnosis:<br>" ++ (show $ intersperse "<br>" $ map show mbt), 2)
         Right pt -> case unSat pt of [] -> ("on the strategy", 0)
                                      un -> ("satisfies:<br>" 
                                             ++ (show $ intersperse "<br>" $ sat pt)
                                             ++ "<br>" ++ "does not satisfy:<br>"
                                             ++ (show $ intersperse "<br>" $ unSat pt), 1)

 


tta :: OrListC 
tta = list2or -- [3*var "x" :=: 8]
  [ 3*(var "x" + 2).^.2 :=: 21] --(var "x" - 10)*(var "x" + 10)]

ttb :: OrListC 
ttb = list2or --[var "x" :=: N 8] -- 
   [(var "x" + 2).^.2 :=: 18] --var "x" ^2 +10]

ttc :: OrListC 
ttc = list2or [ (3*var "x" - 7) :=: N (sqRoot 5), (3*var "x" - 7) :=: N (-sqRoot 5)]

ttd :: OrListC 
ttd = list2or [3 * var "x" ^2 - 49 :=: 5]

tte :: OrListC 
tte = list2or [ (3*var "x" - 7):^:2 - 5 :=: 0]
             --, 2 * var "x" + 10 :=: 4 * (N (sqrtSim 7) + 5 * var "x")
             --, 1 * var "x" + 9 :=: N (sqrtSim 5) + 3 * var "x"]
