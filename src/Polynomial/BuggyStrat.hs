module Polynomial.BuggyStrat where 
import Regulation.Search
import Regulation.Focus
import Regulation.Lift ((>>>))
import FSMMaker.Rules() 
import FSMMaker.StrategyBas hiding (applyAll)
import Polynomial.PolyLift 
import Polynomial.PolyRules
import Polynomial.PolyBuggys
import Polynomial.PolyStrat (evalAndZero)
import SymbolRoot.ComplexRoot hiding (eval)

abcStratB :: Strat (Focus (PQS ComRoot))
abcStratB = 
   R allDeg2
   .*. option (repeatS  (focus (x2x >>> x2s) (R workoutSquareB1 
                                              <|> R workoutSquareB2)))
   .*. repeatS (focus (x2x >>> x2s) (R workoutSquare))
   .*. repeatS (focus (x2x >>> x2s) (R distribute) .*. R nubSub) 
   .*. manyN 3 (focus (tlt >>> x2s) (R negateTerm) .*. R nubSub)
   .*. (R abcAlways <|> R abcDivideBy_a <|> R abcForgetRoot)

divisionB :: Strat (Focus (PEQ ComRoot))
divisionB = 
   R divideByFirst 
   <|> R divideReverse 
   <|> R divideSubtract 
   <|> R divideForget

linSquareStratB :: Strat (Focus (PQS ComRoot))
linSquareStratB = 
      try (R eval)   .*.
      focus e2s (option (R flipEq) .*. try (R transportConstant)    
                    .*. option divisionB)
                    .*. (R linSquare
                         <|> R linSquareForgetRoot 
                         <|> R linSquareForgetRootConst)

nillPruductStratB :: Strat (Focus (PQS ComRoot))
nillPruductStratB = 
      evalAndZero .*.
      focus e2s (option (R flipEq) .*. try (R transportConstant) 
                 .*. option divisionB)
                 .*. (R nillProduct <|> R nillProductB)

linStratB :: Strat (Focus (PQS ComRoot))
linStratB = 
   R allLin 
   .*. repeatS (focus (x2x >>> x2s) (R distribute) .*. R nubSub) 
   .*. manyN 2 (focus (tlt >>> x2s) (R negateTerm) .*. R nubSub)
   .*. R sortAllLin 
   .*. repeatS (focus e2s divisionB)

approximateRootsStat :: Strat (Focus(PQS ComRoot))   
approximateRootsStat = 
   R (roundRootsUp 1) <|> R (roundRootsDown 1) 
   <|> R (roundRootsUp 2) <|> R (roundRootsDown 2)
   <|> R (roundRootsUp 3) <|> R (roundRootsDown 3)  

buggyStrat :: Strat (Focus (PQS ComRoot))
buggyStrat = 
   (linStratB <|> 
      abcStratB <|> (linSquareStratB <|> nillPruductStratB) .*. linStratB)
   .*. R wait 
   .*. R nubSub
   .*. R simplifyAllRoots
   .*. option approximateRootsStat
   .*. R wait 
   .*. R nubSub
   .*. option (R forgetEquation) 
   .*. manyN 2 (focus e2s (R negateSolution))