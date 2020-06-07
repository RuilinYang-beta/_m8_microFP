
-- how to translate a grammar to EDSL?

-- grammar of set 5

{-
⟨expr⟩  ::=  ⟨dec⟩ 
          | ⟨term⟩  
          | ⟨term⟩ (’+’ ⟨expr⟩)* 
          | ⟨if⟩
⟨term⟩  ::=  ⟨factor⟩ 
          |  ⟨factor⟩ (’*’ ⟨term⟩)
⟨factor⟩::= num    
          | identifier ’(’ ⟨expr⟩ ’)’
          | identifier
          | ’(’ ⟨expr⟩ ’)’


⟨condition⟩ ::= ⟨expr⟩ ’==’ ⟨expr⟩
⟨if⟩        ::= ’if’ ⟨condition⟩ ’then’ ⟨expr⟩ ’else’ ⟨expr⟩ 
⟨dec⟩       ::= ’dec’ ⟨expr⟩


⟨function⟩ ::= ’function’ identifier identifier ’=’ ⟨expr⟩

-}

-- Expression EDSL
data Expr = Const Integer
          | Var String
          | Mult Expr Expr
          | Add Expr Expr
          | If Cond Expr Expr
          | Dec Expr
          | FunCall String Expr
          deriving Show

data Cond = Eq Expr Expr
          deriving Show

data FunDef = FunDef String String Expr
            deriving Show



-- Exercise 3-FP.2
languageDef = 
    emptyDef { Token.identStart      = letter, 
               Token.identLetter     = alphaNum, 
               Token.reservedOpNames = ["+", "*", "==", "="], 
               Token.reservedNames   = ["if", "then", "else", "dec", "function"]}

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
integer    = Token.integer lexer
parens     = Token.parens lexer
symbol     = Token.symbol lexer
reserved   = Token.reserved lexer

-- understanding the above functions:
-- identifier :: Parser String 
-- fmap Var identifier, where Var :: String -> Expr
-- result is of type Parser Expr


-- Exercise 3-FP.3 
parseFactor :: Parser Expr
parseFactor = try (Const <$> integer)
          <|> try (FunCall <$> identifier <*> (parens parseExpr))
          <|> try (Var <$> identifier)  
          <|> parens parseExpr

parseTerm :: Parser Expr
-- parseTerm = try (Mult <$> parseFactor <*> (symbol "*" *> parseFactor))
parseTerm = try (Mult <$> parseFactor <*> (symbol "*" *> parseTerm))
          <|> parseFactor

parseExpr :: Parser Expr
parseExpr = try parseDec 
          <|> try (Add <$> parseTerm <*> (symbol"+" *> parseExpr))
          -- <|> try (Add <$> parseTerm <*> (symbol"+" *> parseTerm))
          <|> try parseTerm
          <|> try parseIf
-- parser parseExpr "if a == 9 then 9*(a+d) else dec a"



-- Exercise 3-FP.4 
parseCond :: Parser Cond
parseCond =  Eq <$> parseExpr <*> (reserved "==" *> parseExpr)

parseIf :: Parser Expr
parseIf = If <$> (reserved "if" *> parseCond) <*> (reserved "then" *> parseExpr) <*> (reserved "else" *> parseExpr)

-- parser parseIf "if 1 == (0+1) then a else 1+1"


parseDec :: Parser Expr
parseDec = Dec <$> (reserved "dec" *> parseExpr)


-- Exercise 3-FP.5
parseFunDef :: Parser FunDef
parseFunDef = FunDef <$> (reserved "function" *> identifier) <*> (identifier <* symbol "=") <*> parseExpr

-- parseFuncCall :: Parser Expr -- this functionality is in parseFactor


parserFun :: String -> FunDef
parserFun = parser parseFunDef -- replace parseFun by the name of your parser


-- fib :: FunDef 
-- fib = parserFun
--  "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"


-- Exercise 3-FP.6 
-- trick to evaluate recursive function: 
-- when evalfun first see the function body, 
-- it passes the funcbody, together with others, to evalExpr
-- There's always a funcbody, being remembered but not used in many Expr
-- funcbody is only used when Expr is a Funcall String Expr
evalfun :: FunDef -> Integer -> Integer
evalfun (FunDef f x expr) i = evalExpr i expr funcbody
                              where funcbody = expr

evalExpr :: Integer -> Expr -> Expr -> Integer
evalExpr i (Const c) funcbody             =  c
evalExpr i (Var s) funcbody               =  i
evalExpr i (Mult expr1 expr2) funcbody    = (evalExpr i expr1 funcbody) * (evalExpr i expr2 funcbody)
evalExpr i (Add  expr1 expr2) funcbody    = (evalExpr i expr1 funcbody) + (evalExpr i expr2 funcbody)
evalExpr i (Dec expr) funcbody            = (evalExpr i expr funcbody) - 1
evalExpr i (If (Eq e1 e2) expr1 expr2) funcbody | evalExpr i e1 funcbody == evalExpr i e2 funcbody  = evalExpr i expr1 funcbody 
                                                | otherwise                                         = evalExpr i expr2 funcbody 
evalExpr i (FunCall s expr) funcbody      =  evalExpr (evalExpr i expr funcbody) funcbody funcbody


-- non-recursive functions
-- testFun = parser parseFunDef "function f x = if 3 == 4 then x + 1 else dec x"  -- of type FunDef
-- testFun = parser parseFunDef "function f x = x*x + 2*x + 1" 
-- testFun = parser parseFunDef "function something x = x + 2"

-- recursive functions
testFun = parser parseFunDef "function factorial x = if x == 0 then 1 else factorial(dec x) * x"

testEvalFun = evalfun testFun 6


-- Checks if in the evaluation (xˆ2 + 2*x + 1) == (x+1)ˆ2
fn = (evalfun . parserFun) "function f x = x*x + 2*x + 1" 
fn' = (evalfun . parserFun) "function f x = (x+1) * (x+1)" 
prop_fn n = n >= 0 ==> fn n == fn' n


-- this is not correct, evalExpr i (FunCall s expr) should call evalfun
factorial :: Integer -> Integer
factorial = (evalfun . parserFun) "function factorial x = if x == 0 then 1 else factorial(dec x) * x"
prop_factorial n = n >= 0 ==> factorial n == product [1..n]


fib :: Integer -> Integer
fib = (evalfun . parserFun)
 "function fib x = if x == 0 then 1 else (if x == 1 then 1 else fib(dec x)+fib(dec dec x))"











