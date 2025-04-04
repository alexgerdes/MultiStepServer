module Polynomial.PolyStrat where 
import Regulation.Search
import Regulation.Focus 
import Regulation.Lift ((>>>))
import FSMMaker.Rules() 
import FSMMaker.StrategyBas hiding (applyAll)
import FSMMaker.FSM (fsm)

import Polynomial.PolyLift 
import Polynomial.PolyRules

-- imports for distset
import Polynomial.ExprPoly hiding (rebracket)
import qualified Regulation.Tracer as T
import SymbolRoot.ComplexRoot hiding (eval)

distSet :: OrList String ComRoot -> [OrList String ComRoot]
distSet p = 
   map (pqs . top . T.element . fst) $
   derivations fsm' $ embed $ PQS p
   where fsm' = fsm $ 
                fmap T.neutral2tracer distributeStrat

evalAndZero :: Strat (Focus (PQS ComRoot))
evalAndZero = 
   try (R eval) 
   .*. try (focus e2s (R derive2zero)) 

distributeStrat :: Strat (Focus (PQS ComRoot)) 
distributeStrat = 
   repeatS (focus (x2x >>> x2s) (R workoutSquare))
   .*. repeatS  (focus (x2x >>> x2s) (R distribute) .*. R nubSub)
   .*. evalAndZero
   .*. try (R rebracket)
   .*. try (R evalMinor)
   .*. R wait
   .*. R nubSub

factorStrat :: Strat (Focus (PQS ComRoot))
factorStrat =  
      evalAndZero
      .*. focus (tlf >>> tlt >>> x2s) (try (R workoutSquare)) 
      .*. focus e2s (focus x2e (R permutateFactors 
                                .*. R factor))
      .*. R evalMinor
      .*. R wait
      .*. R nubSub
      .*. try (focus (tlt >>> tlf >>> x2s) (R distribute))
      .*. try (R rebracket)                    
      .*. R nillProduct
      .*. try (R removeDegen)

linSquareStrat :: Strat (Focus (PQS ComRoot))
linSquareStrat = 
    R eval .*.
    focus e2s (option (R flipEq)
                 .*. try (R transportConstant) 
                 .*. option (R divideByFirst))
                 .*. R linSquare

factorThreeTermStrat :: Strat (Focus (PQS ComRoot))
factorThreeTermStrat =
         focus e2s ( try (R factorLead) .*. option (R divideByFirst) 
                     .*. focus (tlf >>> x2e) (R factorThreeTerm) 
                     .*. try (R divideByFirst))
         .*. R evalMinor
         .*. R wait
         .*. R nubSub
         .*. R nillProduct 

completeSquareStrat :: Strat (Focus (PQS ComRoot))
completeSquareStrat = 
   evalAndZero 
   .*. focus e2s (try (R factorLead) .*. option (R divideByFirst) 
   .*. focus (tlf >>> x2e) (R completeSquare) 
   .*. option (focus (tlt >>> x2e) (R distribute))
   .*. try (R divideByFirst))
   .*. linSquareStrat

nillProductStrat :: Strat (Focus (PQS ComRoot))
nillProductStrat = 
   evalAndZero
   .*. focus e2s (try (R divideByFirst))
   .*. R nillProduct

abcStrat :: Strat (Focus (PQS ComRoot))
abcStrat = 
   option (focus e2s (R factorLead .*. R divideByFirst))
   .*. R abcFormula

linStrat :: Strat (Focus (PQS ComRoot))
linStrat =   
   R allLin 
   .*. distributeStrat
   .*. try (R rebracket)
   .*. try (R evalMinor)
   .*. R wait
   .*. R nubSub
   .*. repeatS (focus e2s (R sortLin))
   .*. repeatS (focus e2s (R divideByFirst))

polyStrat :: Strat (Focus (PQS ComRoot))
polyStrat = 
  linStrat |> 
      (factorStrat' <|> nillProductStrat' <|> linSquareStrat') 
        .*. linStrat
           |> distributeStrat 
           .*. (factorThreeTermStrat' 
               .*. linStrat |> abcStrat <|> completeSquareStrat .*. linStrat)
   where factorStrat'          = (testStrat factorStrat) 
                                 .*. factorStrat 
         nillProductStrat'     = (testStrat nillProductStrat)
                                 .*. nillProductStrat
         linSquareStrat'       = (testStrat linSquareStrat) 
                                 .*. linSquareStrat
         factorThreeTermStrat' = (testStrat factorThreeTermStrat) 
                                 .*. (factorThreeTermStrat <|> completeSquareStrat)