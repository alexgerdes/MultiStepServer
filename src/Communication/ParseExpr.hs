module Communication.ParseExpr where 
import Text.Parsec
import Text.Parsec.Pos (newPos)
import Text.Parsec.Error -- (messageString, errorMessages)
import Text.Parsec.String (Parser)
import Text.Parsec.Token hiding 
   (integer, natural, parens, identifier, symbol, whiteSpace)
import qualified Text.Parsec.Token as T 
import Text.Parsec.Language (haskellDef)
import Polynomial.ExprPoly 
   ( ExprPoly, PolyEq ((:=:)), simplify
   , (.^.), mapTerms, OrList, list2or)
import SymbolRoot.ComplexRoot (ComRoot)
import qualified SymbolRoot.ComplexRoot as CR
import qualified Polynomial.ExprPoly as EP   

-- Define the Expr datatype
data Expr = Expr :+: Expr
          | Expr :-: Expr
          | Expr :*: Expr
          | Expr :/: Expr
          | Negate Expr
          | Sqrt Expr
          | Expr :^: Expr
          | N Integer
          | D Double
          | Var String
   deriving (Eq, Show)

lexer :: TokenParser ()
lexer = makeTokenParser haskellDef

whiteSpace :: Parser ()
whiteSpace = T.whiteSpace lexer

integer :: Parser Integer
integer = do{symbol "-"; n <- natural; return (-n)} 
          <|> natural 
   where natural = T.natural lexer

double :: Parser Double
double = do {symbol "-"; n <- double'; return (-n)} 
          <|> double'
   where double' = float lexer

identifier :: Parser String
identifier = T.identifier lexer

parens :: Parser a -> Parser a
parens = T.parens lexer

symbol :: String -> Parser String
symbol = T.symbol lexer 

sqrtP :: Parser Expr
sqrtP = do symbol "sqrt"
           x <- parens expr
           return (Sqrt x)

sqrtN :: Parser Expr
sqrtN = do symbol "-"
           symbol "sqrt"
           x <- parens expr
           return (N(-1) :*: Sqrt x)

neg :: Parser Expr
neg = do symbol "-"
         x <- expr
         return (Negate x) 

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = expo `chainr1` mulop

expo :: Parser Expr
expo = factor `chainl1` expop

atom :: Parser Expr
atom = (Var <$> identifier)
      <|> try (D <$> double)
      <|> (N <$> integer)

factor :: Parser Expr
factor = parens expr 
         -- <|> try sqrtN
          <|> try sqrtP
          <|> try atom
          <|> neg 
          
expop :: Parser (Expr -> Expr -> Expr)
expop = do {symbol "^"; return (:^:)} 

mulop :: Parser (Expr -> Expr -> Expr)
mulop = do{ symbol "*"; return (:*:) }
    <|> do{ symbol "/"; return (:/:) }
    <|> do{ symbol ""; notFollowedBy (symbol "-"); return (:*:) }


addop :: Parser (Expr -> Expr -> Expr)
addop = do{ symbol "+"; return (:+:) }
    <|> do{ symbol "-"; return (:-:) }

       
parseExpr :: String -> Either ParseError Expr
parseExpr = parse ( whiteSpace *> expr <* eof) ""

makePoly :: Expr -> Either String (ExprPoly String ComRoot)
makePoly p = let simp = fmap CR.simplify . simplify  
   in case p of 
    a :+: b -> (+) <$> makePoly a <*> makePoly b
    a :-: b -> (-) <$> makePoly a <*> makePoly b
    a :*: b -> (*) <$> makePoly a <*> makePoly b
    a :/: b -> case simp <$> makePoly b of 
                  Right (EP.N r) | r /= 0 -> 
                     (mapTerms (EP.N (CR.simplify $ 1/r)*)) <$> (makePoly a)
                  _ -> Left "division by a non-number"
    a :^: b -> case b of N n -> (.^. (fromIntegral n)) <$> (makePoly a)
                         _ -> Left "exponent is not a number"
    Sqrt a -> case simp <$> makePoly a of 
                  Right (EP.N r) | r /= 0 -> Right $ EP.N (CR.sqRoot r) 
                  _ -> Left "sqrt of a non-number"
    Negate a -> negate <$> makePoly a
    N a -> Right $ EP.N $ fromIntegral a
    D d -> Right $ EP.N $ fromRational $ toRational d
    Var x -> Right $ EP.Var x


replace :: String -> String -> String -> String
replace _ _ [] = []
replace old new (x:xs) = 
   if take n (x:xs) == old 
   then new ++ replace old new (drop n (x:xs)) 
   else x:replace old new xs 
   where n = length old

repairSquare :: String -> String
repairSquare  = replace "²" "^2"

repairSum :: String -> String
repairSum = replace "%2B" "+"

parse2poly :: String -> Either String (ExprPoly String ComRoot)
parse2poly s = case parseExpr $ repairSum s of 
   Right p  -> makePoly p  
   Left msg -> Left $ show msg

parse2eq :: String -> Either String (PolyEq String ComRoot)
parse2eq p = case break (=='=') p of 
   (lhs,'=':rhs) -> (:=:) <$> (parse2poly lhs) <*> (parse2poly rhs) 
   _ -> Left "not an equation"

breakAt :: String -> String -> (String, String)
breakAt _ [] = ([],[])
breakAt (x:xs) p = let (l, r) = break (==x) p in 
   if take n r == (x:xs) then (l, drop n r) 
   else let (l', r') = breakAt (x:xs) (drop n r) 
        in (l ++ take n r ++ l', r')
   where n = length (x:xs)

parse2or :: String -> Either String (OrList String ComRoot)
parse2or p = case breakAt "of" p of 
   ("",_) -> Left "invalid input"
   (s,"") -> (list2or . (:[])) <$> (parse2eq s) 
   (s, t) -> f <$> (parse2eq s) <*> (parse2eq t)
      where f x y = list2or [x, y]




   