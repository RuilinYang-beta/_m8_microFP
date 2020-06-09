-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Ruilin Yang (s2099497)
-- Student 2: Joanna Rostek (s2459698)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All


-- ---------- FP3.1 ----------

{-
data Func - describes functions
-}
data Func = Assign String [Expr] Expr   deriving (Show, Eq)

{-
data Expr - describes expressions
-}
data Expr = Constant   Integer
          | Var        String
          | Add        Expr Expr
          | Sub        Expr Expr
          | Mul        Expr Expr
          | FunCall    String [Expr]
          | If         Cond Expr Expr
          deriving (Show, Eq)

{-
data Cond - describes 'if' statement conditions
-}
data Cond = Cond Expr Order Expr
          deriving (Show, Eq)

{-
data Order - describes order types
-}
data Order = Gt | Eq | Lt
           deriving (Eq)

{-
Order is an instance of Show - shows order types as their mathematical representation
-}
instance Show Order where
    show Gt = ">"
    show Eq = "=="
    show Lt = "<"


-- ---------- FP3.2 ----------

{-
defining functions corresponding to the ones in "functions.txt"
-}
fibonacci = Assign "fibonacci" [(Var "n")] (Add (FunCall "fibonacci" [(Sub (Var "n") (Constant 1))]) (FunCall "fibonacci" [(Sub (Var "n") (Constant 2))]) )

fib = Assign "fib" [(Var "n")] (If (Cond (Var "n") Lt (Constant 3)) (Constant 1) (Add (FunCall "fib" [(Sub (Var "n") (Constant 1))]) (FunCall "fib" [(Sub (Var "n") (Constant 2))])))

sum' = Assign "sum" [(Var "a")] (Add (FunCall "sum" [(Sub (Var "a") (Constant 1))]) (Var "a"))

div' = Assign "div" [(Var "x"), (Var "y")] (If (Cond (Var "x") Lt (Var "y")) (Constant 0) (Add (Constant 1) (FunCall "div" [(Sub (Var "x") (Var "y")), (Var "y")])))

twice = Assign "twice" [(Var "f"), (Var "x")] (FunCall "f" [(FunCall "f" [(Var "x")])])

add = Assign "add" [(Var "x"), (Var "y")] (Add (Var "x") (Var "y"))

inc = Assign "inc" [] (FunCall "add" [(Constant 1)])

eleven = Assign "eleven" [] (FunCall "inc" [(Constant 10)])


-- ---------- FP3.3 ----------

-- ----- tier 0: the top-most level -----

{-
    `pretty` takes care of things of `Func` data type
    `oneExpr` and `manyExpr` deals with things of `Expr` type
-}
pretty :: Func -> String 
pretty (Assign funcname args funcbody) = funcname ++ " " ++ argString ++ " := "++ funcbodyString ++ ";"
    where argString      = manyExpr args " "
          funcbodyString = oneExpr funcbody


-- ----- tier 1: helpers of `pretty`: `manyExpr`, `oneExpr`, `manyExprHelper`, `condString` -----
{-
    transform a list of Expr into strings separated by whitespace
    Params: 
    1 [Expr]: the list of expression to be stringified
    2 String: the separator, in case of function definition it should be " "; 
                           in case of function call it should be ", ".
    3 String: the final result
-}
manyExpr :: [Expr] -> String -> String 
manyExpr expr sep = trim toTrim
    where toTrim = manyExprHelper expr sep
          trim   = (\input -> let len = (length input) - (length sep) in   -- trim the trailing separator
                              take len input)

manyExprHelper :: [Expr] -> String -> String 
manyExprHelper [] sep     = []
manyExprHelper (x:xs) sep = (oneExpr x) ++ sep ++ (manyExprHelper xs sep) 


oneExpr :: Expr -> String
oneExpr (Constant c) = show c 
oneExpr (Var v)      = v
oneExpr (Add e1 e2)  = (oneExpr e1) ++ " + " ++ (oneExpr e2)
oneExpr (Sub e1 e2)  = (oneExpr e1) ++ " - " ++ (oneExpr e2) 
oneExpr (Mul e1 e2)  = (oneExpr e1) ++ " * " ++ (oneExpr e2) 
oneExpr (FunCall fname args)  = fname ++ 
                                " (" ++ 
                                (manyExpr args ", ") ++ -- args in FunCall should be separated by comma 
                                ")"  
oneExpr (If cond eThen eElse) =  "if " ++ 
                                 (condString cond) ++ 
                                 " then {\n\t" ++ 
                                 (oneExpr eThen) ++ 
                                 "\n} else {\n\t" ++ 
                                 (oneExpr eElse) ++
                                 "\n}"
{-
    helper of `oneExpr`
    resulting in a string of condition surrounded by ()
-}
condString :: Cond -> String 
condString (Cond e1 ord e2) = "(" ++ (oneExpr e1) ++ " " ++ (show ord) ++ " " ++ (oneExpr e2) ++ ")"


-- ---------- FP3.4 ----------


{-
    about functions: 
        basic evaluation:         `fib` `div` `add`    can evaluate
        need pattern matching:    `sum` `fibonacci`    doesn't support 
        need partial application: `inc`                doesn't support
        need higher order func:   `twice`              doesn't support
-}

-- ----- tier 0: the top-most level -----
eval :: Func -> [Expr] -> [Expr] -> Expr 
eval (Assign fname argsF fbody) args vals = reduceOneExpr (bindOneExpr fbody args vals) func 
    where func = Assign fname argsF fbody


-- ----- tier 1: helpers of tier 0 functions -----
{-
    Params: 
    1 Expr:   the expression containing variables
    2 [Expr]: the variables of concern
    3 [Expr]: the values to be bind to the variables
    4 Expr:   resulting expression
-}
bindOneExpr :: Expr -> [Expr] -> [Expr] -> Expr
bindOneExpr (Constant i)  _ _       = Constant i
bindOneExpr (Var v) args vals       | lresult == Nothing     = Var v
                                    | otherwise              = extractMaybe lresult
                                    where mapping = zip args vals
                                          lresult = lookup (Var v) mapping 
                                          extractMaybe = (\(Just a) -> a)
bindOneExpr (Add e1 e2) args vals   = (Add (bindOneExpr e1 args vals) (bindOneExpr e2 args vals))
bindOneExpr (Sub e1 e2) args vals   = (Sub (bindOneExpr e1 args vals) (bindOneExpr e2 args vals))
bindOneExpr (Mul e1 e2) args vals   = (Mul (bindOneExpr e1 args vals) (bindOneExpr e2 args vals))
bindOneExpr (FunCall s argsF) args vals = FunCall s (bindListExpr argsF args vals)
bindOneExpr (If (Cond e1 order e2) eThen eElse) args vals = (If (Cond e1' order e2') eThen' eElse')
    where [e1', e2', eThen', eElse'] = bindListExpr [e1, e2, eThen, eElse] args vals


{-
    Params: 
    1 [Expr]: the list of expressions that contain variables 
    2 [Expr]: the variables of concern
    3 [Expr]: the values to be bind to the variables 
    4 [Expr]: the resulting list of Expr after binding
-}
bindListExpr :: [Expr] -> [Expr] -> [Expr] -> [Expr]
bindListExpr [] _ _           = []
bindListExpr (x:xs) args vals = bindOneExpr x args vals : bindListExpr xs args vals


reduceOneExpr :: Expr -> Func -> Expr
reduceOneExpr (Constant c)  func = Constant c
reduceOneExpr (Var v)       func = Var v
reduceOneExpr (Add (Constant c1) (Constant c2))  func  = Constant (c1+c2)
reduceOneExpr (Add e1 e2)                        func  = Add (reduceOneExpr e1 func) (reduceOneExpr e2 func)
reduceOneExpr (Sub (Constant c1) (Constant c2))  func  = Constant (c1-c2)
reduceOneExpr (Sub e1 e2)                        func  = Sub (reduceOneExpr e1 func) (reduceOneExpr e2 func)
reduceOneExpr (Mul (Constant c1) (Constant c2))  func  = Constant (c1*c2)
reduceOneExpr (Mul e1 e2)                        func  = Mul (reduceOneExpr e1 func) (reduceOneExpr e2 func)
reduceOneExpr (FunCall s args) (Assign fname argsF fbody)  | funNoVar   =  reduceOneExpr (bindOneExpr fbody argsF reducedArgs) func 
                                                           | otherwise  =  reduced
                                                           where func        = Assign fname argsF fbody
                                                                 reducedArgs = reduceListExpr args func
                                                                 reduced     = FunCall s reducedArgs
                                                                 funNoVar    = hasNoVarExpr reduced 
reduceOneExpr (If (Cond e1 order e2) eThen eElse) func | condNoVar && condTrue      = reduceOneExpr eThen' func
                                                       | condNoVar && not(condTrue) = reduceOneExpr eElse' func
                                                       | otherwise                  = reduced
                                                       where reduced   = (If (Cond e1' order e2') eThen' eElse')
                                                             [e1', e2', eThen', eElse'] = reduceListExpr [e1, e2, eThen, eElse] func
                                                             condNoVar = hasNoVarCond (Cond e1' order e2')
                                                             condTrue  = evalCond (Cond e1' order e2')

reduceListExpr :: [Expr] -> Func -> [Expr]
reduceListExpr [] _        = []
reduceListExpr (x:xs) func = reduceOneExpr x func : reduceListExpr xs func


-- ----- tier 2: helpers of tier 1 functions -----

hasNoVarExpr :: Expr -> Bool
hasNoVarExpr (Constant i)  = True
hasNoVarExpr (Var v)       = False
hasNoVarExpr (Add e1 e2)   = (hasNoVarExpr e1) && (hasNoVarExpr e2)
hasNoVarExpr (Sub e1 e2)   = (hasNoVarExpr e1) && (hasNoVarExpr e2)
hasNoVarExpr (Mul e1 e2)   = (hasNoVarExpr e1) && (hasNoVarExpr e2)
hasNoVarExpr (FunCall s args) = all hasNoVarExpr args
hasNoVarExpr (If (Cond e1 od e2) eThen eElse) = all hasNoVarExpr [e1, e2, eThen, eElse]


hasNoVarCond :: Cond -> Bool 
hasNoVarCond (Cond e1 od e2) = all hasNoVarExpr [e1, e2]


evalCond :: Cond -> Bool 
evalCond (Cond e1 od e2) | od == Gt   = (evalExpr e1) >  (evalExpr e2)
                         | od == Eq   = (evalExpr e1) == (evalExpr e2) 
                         | otherwise  = (evalExpr e1) <  (evalExpr e2)

evalExpr :: Expr -> Integer 
evalExpr (Constant i) = i



-- ---------- FP4.1 ----------
{-
func - parses functions
-}
func :: Parser Func
func = pure Assign <*> identifier <*> sep idOrInt (symbol "") <* (symbol ":=") <*> expr <* (symbol ";")

{-
idOrInt - helper function that parses an identifier or an integer
-}
idOrInt = pure Var <*> identifier
      <|> pure Constant <*> integer

{-
term - parses multiplication or goes to factor
-}
term :: Parser Expr
term = pure Mul <*> factor <*> (symbol "*" *> term)
   <|> factor

{-
expr - parses an expression - addition or subtraction, or goes to term
-}
expr :: Parser Expr
expr =  pure Add <*> term <*> (symbol "+" *> expr)
    <|> pure Sub <*> term <*> (symbol "-" *> expr)
    <|> term

{-
cond - parses a condition
-}
cond :: Parser Cond
cond = pure Cond <*> expr <*> order <*> expr

{-
order - parses the order symbols
-}
order ::Parser Order
order = (pure Gt <* symbol ">" )
    <|> (pure Eq <* symbol "==")
    <|> (pure Lt <* symbol "<" )

{-
factor - parses integers, 'if'-statements (through parseIf), function calls, identifiers or expressions in parentheses
-}
factor :: Parser Expr
factor = (pure Constant <*> integer)
      <|> parseIf
      <|> pure FunCall <*> identifier <*> parens (sep1 expr (symbol ","))
      <|> (pure Var <*> identifier)
      <|> parens expr

{-
parseIf - parses 'if'-statements
-}
parseIf :: Parser Expr
parseIf = pure If <*> (between (symbol "if") (parens cond) (symbol "then")) <*> braces expr <* (symbol "else") <*> braces expr


-- ---------- test section ----------

-- ----- test FP3.3 -----
{-
the following will print out a readable version of functions defined in FP3.3
-}
pFibo  = putStrLn $ pretty fibonacci
pFib   = putStrLn $ pretty fib
pSum'  = putStrLn $ pretty sum' 
pDiv'  = putStrLn $ pretty div'
pTwice = putStrLn $ pretty twice
pAdd   = putStrLn $ pretty add 
pInc   = putStrLn $ pretty inc 
pEleven = putStrLn $ pretty eleven


-- ----- test FP3.4 -----

-- evaluation of `fib` `div` and `add`, when all arguments are supplied with value

-- eval `fib
argFib = [Var "n"]
valEvalFib = [Constant 10]

testEvalFib :: Expr
testEvalFib = eval func argFib valEvalFib
    where func = fib

-- eval `div`
argDiv = [(Var "x"), (Var "y")]
valEvalDiv = [Constant 66, Constant 15]

testEvalDiv :: Expr
testEvalDiv = eval func argDiv valEvalDiv
    where func = div'

-- eval `add`
argAdd      = [(Var "x"), (Var "y")]
valEvalAdd  = [Constant 99, Constant 10]

testEvalAdd :: Expr
testEvalAdd = eval func argAdd valEvalAdd
    where func = add

-- ----- Test FP4.1 ------

{-
running tests: testSomeFunctionName (Stream "input to parse")
-}

testFactor = runParser $ factor    -- will parse integers, 'if'-statements, function calls, identifiers or expressions in parentheses

testExpr = runParser $ expr        -- will parse addition, substraction or terms

testTerm = runParser $ term        -- will parse multiplication or factors

testCond = runParser $ cond        -- will parse conditions, e.g. "a>b"

testFunc = runParser $ func        -- will parse function definitions

testOrder = runParser $ order      -- will parse order, e.g. "=="

{-
the following tests can be combined with their counterparts in the block
below in order to get both the parsed, as well as the readable version
of a function. Example:

    > testFibo
    [(Assign "fibonacci" [Var "n"] (Add (FunCall "fibonacci" [Sub (Var "n") (Constant 1)]) (FunCall "fibonacci" [Sub (Var "n") (Constant 2)])),Stream "")]

    > testFibo'
    "fibonacci n := fibonacci (n - 1) + fibonacci (n - 2);"
-}
{-
from String to parsing result 
-}
testFibo    = testFunc $ Stream $ pretty fibonacci
testFib     = testFunc $ Stream $ pretty fib
testSum     = testFunc $ Stream $ pretty sum'
testDiv     = testFunc $ Stream $ pretty div'
testTwice   = testFunc $ Stream $ pretty twice
testAdd     = testFunc $ Stream $ pretty add
testInc     = testFunc $ Stream $ pretty inc
testEleven  = testFunc $ Stream $ pretty eleven


{-
from parsed result to String
-}
testFibo'  = pretty $ fst $ head $ testFibo
testFib'   = pretty $ fst $ head $ testFib
testSum'   = pretty $ fst $ head $ testSum
testDiv'   = pretty $ fst $ head $ testDiv
testTwice' = pretty $ fst $ head $ testTwice
testAdd'   = pretty $ fst $ head $ testAdd
testInc'   = pretty $ fst $ head $ testInc
testEleven'= pretty $ fst $ head $ testEleven



