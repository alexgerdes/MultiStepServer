module FSMMaker.Rules 
   ( Rule
   , rule
   , defaultRule
   , buggyRule  
   , minorRule
   , isBuggy
   , isMinor
   , ruleName
   , applyRule
   , notR
   , test) where 
data Rule a = 
   Rule { isBuggy   :: Bool
        , isMinor   :: Bool
        , ruleName  :: String
        , applyRule :: a -> [a]}

instance Show (Rule a) where 
   show r = ruleName r

defaultRule :: Rule a
defaultRule = Rule False False "" (:[])

rule :: String -> (a -> [a]) -> Rule a
rule = Rule False False 

buggyRule :: String -> (a -> [a]) -> Rule a
buggyRule = Rule True False

minorRule :: String -> (a -> [a]) -> Rule a
minorRule = Rule False True 

notR :: Rule a -> Rule a 
notR (Rule _ _ s f) = minorRule s' f' 
   where 
      f' a = case f a of 
         [] -> [a]
         _  -> []
      s' = "not " ++ s

test :: (a -> Bool) -> String -> Rule a 
test p s = minorRule s f
   where 
      f a
       | p a = [a]
       | otherwise = []  