-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Your Name (sxxxxxxx)
-- Student 2: Other Name (syyyyyyy)

{-# LANGUAGE TemplateHaskell #-}

module MicroFP where

import Control.Applicative
import PComb
import BasicParsers
import Test.QuickCheck
import Test.QuickCheck.All


-- ---------- FP3.1 ----------

data Func = Assign String [Expr] Expr   deriving (Show, Eq)-- in the last there's a ;

data Expr = Constant   Integer
          | Var        String
          -- these here are up for deleting as soon as we fix their usages
          | Add        Expr Expr
          | Sub        Expr Expr
          | Mul        Expr Expr
          -- down to here
          | FunCall    String [Expr]
          | If         Cond Expr Expr
          | BinaryOp   Expr Op Expr
          | Mult       Expr Op Expr
          deriving (Show, Eq)

data Op = Adda | Suba | Mula
          deriving Eq

instance Show Op where
    show Adda = "+"
    show Suba = "-"
    show Mula = "*"

data Cond = Cond Expr Order Expr
          deriving (Show, Eq)

data Order = Gt | Eq | Lt
           deriving (Eq)


instance Show Order where
    show Gt = ">"
    show Eq = "=="
    show Lt = "<"


-- ---------- FP3.2 ----------

fibonacci = Assign "fibonacci" [(Var "n")] (Add (FunCall "fibonacci" [(Sub (Var "n") (Constant 1))]) (FunCall "fibonacci" [(Sub (Var "n") (Constant 2))]) )

fib = Assign "fib" [(Var "n")] (If (Cond (Var "n") Lt (Constant 3)) (Constant 1) (Add (FunCall "fib" [(Sub (Var "n") (Constant 1))]) (FunCall "fib" [(Sub (Var "n") (Constant 2))])))

sum' = Assign "sum" [(Var "a")] (Add (FunCall "sum" [(Sub (Var "a") (Constant 1))]) (Var "a"))

div' = Assign "div" [(Var "x"), (Var "y")] (If (Cond (Var "x") Lt (Var "y")) (Constant 0) (Add (Constant 1) (FunCall "div" [(Sub (Var "x") (Var "y")), (Var "y")])))

twice = Assign "twice" [(Var "f"), (Var "x")] (FunCall "f" [(FunCall "f" [(Var "x")])])

add = Assign "add" [(Var "x"), (Var "y")] (Add (Var "x") (Var "y"))

inc = Assign "inc" [] (FunCall "add" [(Constant 1)])

eleven = Assign "eleven" [] (FunCall "inc" [(Constant 10)])


-- ---------- FP3.3 ----------
pretty :: Func -> String
pretty (Assign funcname args funcbody) = funcname ++ " " ++ argString ++ " := "++ funcbodyString ++ ";"
    where argString      = manyExpr args " "
          funcbodyString = oneExpr funcbody


-- transform a list of Expr into strings separated by whitespace
-- Params:
-- 1 [Expr]: the list of expression to be stringified
-- 2 String: the separator, in case of function definition it should be " ";
--                        in case of function call it should be ", ".
-- 3 String: the final result
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
                                (manyExpr args ", ") ++ -- args here should be separated by comma
                                ")"
oneExpr (If cond eThen eElse) =  "if " ++
                                 (condString cond) ++
                                 " then {\n\t" ++
                                 (oneExpr eThen) ++
                                 "\n} else {\n\t" ++
                                 (oneExpr eElse) ++
                                 "\n}"


-- resulting in a string of condition surrounded by ()
condString :: Cond -> String
condString (Cond e1 ord e2) = "(" ++ (oneExpr e1) ++ " " ++ (show ord) ++ " " ++ (oneExpr e2) ++ ")"


-- ---------- FP3.4 ----------

temp = zip [(Var "x"), (Var "y")] [(Constant 1),(Constant 2)]

-- does it need a db to store multiple functions??????
{-
-- about functions:
	basic evaluation:         `fib` `div` `add` maybe `main`
	need pattern matching:    `sum` `fibonacci`
	need partial application: `inc`
	need higher order func:   `twice`
-}
-- eval :: Func -> [Expr] -> [Expr] -> Integer
-- eval (Assign funcname args funcbody) argsVal = something

-- Params:
-- 1 Expr:   the expression containing variables
-- 2 [Expr]: the variables of concern
-- 3 [Expr]: the values to be bind to the variables
-- 4 Expr:   resulting expression
-- Requires:
-- length (2 [Expr]) == length (3 [Expr])
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




-- Params:
-- 1 [Expr]: the list of expressions that contain variables
-- 2 [Expr]: the variables of concern
-- 3 [Expr]: the values to be bind to the variables
-- 4 [Expr]: the resulting list of Expr after binding
bindListExpr :: [Expr] -> [Expr] -> [Expr] -> [Expr]
bindListExpr [] _ _           = []
bindListExpr (x:xs) args vals = bindOneExpr x args vals : bindListExpr xs args vals



-- ???????????? how should `reduce` make use of `bind`??
-- might have something to do with recursive function
reduceOneExpr :: Expr -> Expr
reduceOneExpr (Constant c)   = Constant c
reduceOneExpr (Var v)        = Var v
reduceOneExpr (Add (Constant c1) (Constant c2))    = Constant (c1+c2)
reduceOneExpr (Add e1 e2)                          = Add (reduceOneExpr e1) (reduceOneExpr e2)
reduceOneExpr (Sub (Constant c1) (Constant c2))    = Constant (c1-c2)
reduceOneExpr (Sub e1 e2)                          = Sub (reduceOneExpr e1) (reduceOneExpr e2)
reduceOneExpr (Mul (Constant c1) (Constant c2))    = Constant (c1*c2)
reduceOneExpr (Mul e1 e2)                          = Mul (reduceOneExpr e1) (reduceOneExpr e2)
reduceOneExpr (FunCall s args)                     = FunCall s (reduceListExpr args)
reduceOneExpr (If (Cond e1 order e2) eThen eElse)  = (If (Cond e1' order e2') eThen' eElse')
    where [e1', e2', eThen', eElse'] = reduceListExpr [e1, e2, eThen, eElse]


reduceListExpr :: [Expr] -> [Expr]
reduceListExpr [] = []
reduceListExpr (x:xs) = reduceOneExpr x : reduceListExpr xs

-- ---------- FP4.1 ----------

func :: Parser Func
func = pure Assign <*> identifier <*> sep1 idOrInt (symbol ",") <* (symbol ":=") <*> expr <* (symbol ";")

idOrInt = pure Var <*> identifier
      <|> pure Constant <*> integer

term :: Parser Expr
term = pure Mult <*> factor <*> mult <*> term
   <|> factor

mult :: Parser Op
mult = pure Mula <* symbol "*"

expr :: Parser Expr
expr = pure BinaryOp <*> term <*> op <*> expr
   <|> term

op :: Parser Op
op = pure Adda <* symbol "+"
 <|> pure Suba <* symbol "-"

cond :: Parser Cond
cond = parens condHelper

condHelper :: Parser Cond
condHelper = pure Cond <*> expr <*> order <*> expr

order ::Parser Order
order = (pure Gt <* symbol ">" )
    <|> (pure Eq <* symbol "==")
    <|> (pure Lt <* symbol "<" )

factor :: Parser Expr
factor = pure If <*> (between (symbol "if") cond (symbol "then")) <*> braces expr <* (symbol "else") <*> braces expr
     <|> (parens expr)
     <|> (pure Var <*> identifier)
     <|> (pure Constant <*> integer)





-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll

-- ---------- temp section ----------

-- ----- test FP3.3 -----
pFibo  = putStrLn $ pretty fibonacci
pFib   = putStrLn $ pretty fib
pSum'  = putStrLn $ pretty sum'
pDiv'  = putStrLn $ pretty div'
pTwice = putStrLn $ pretty twice
pAdd   = putStrLn $ pretty add
pInc   = putStrLn $ pretty inc
pEleven = putStrLn $ pretty eleven


-- ----- test FP3.4 -----
bindXYZ = [(Var "x"), (Var "y"), (Var "z")]
bindXY  = [(Var "x"), (Var "y")]
bindXZ  = [(Var "x"), (Var "z")]
bindX   = [(Var "x")]
val3    = [(Constant 10), (Constant 20), (Constant 30)]
val2    = [(Constant 10), (Constant 20)]
val1    = [(Constant 10)]

testBindConst = putStrLn $ oneExpr $ bindOneExpr (Constant 99) bindX val3
testBindVar   = putStrLn $ oneExpr $ bindOneExpr (Var "x") bindXY val2
testBindVar'  = putStrLn $ oneExpr $ bindOneExpr (Var "z") bindXY val2
testBindAdd   = putStrLn $ oneExpr $ bindOneExpr (Add (Var "x") (Var "y")) bindXY val3
testBindSub   = putStrLn $ oneExpr $ bindOneExpr (Sub (Var "x") (Var "z")) bindXY val2
testBindMul   = putStrLn $ oneExpr $ bindOneExpr (Mul (Constant 99) (Var "y")) bindXY val2

testBindFunCallRaw :: Expr
testBindFunCallRaw = bindOneExpr addfib bindXY val2
    where addfib = (Add (FunCall "fibonacci" [(Sub (Var "x") (Constant 1))]) (FunCall "fibonacci" [(Sub (Var "x") (Constant 2))]) )
testBindFunCall :: IO ()
testBindFunCall = putStrLn $ oneExpr $ testBindFunCallRaw

testBindIfRaw :: Expr
testBindIfRaw = bindOneExpr ifexpr bindXY val2
    where ifexpr = (If (Cond (Var "x") Lt (Var "y")) (Constant 0) (Add (Constant 1) (FunCall "div" [(Sub (Var "x") (Var "y")), (Var "y")])))
testBindIf :: IO ()
testBindIf = putStrLn $ oneExpr testBindIfRaw


testReduceFunRaw :: Expr
testReduceFunRaw = reduceOneExpr testBindFunCallRaw
testReduceFun :: IO ()
testReduceFun = putStrLn $ oneExpr $ testReduceFunRaw

testReduceIfRaw :: Expr
testReduceIfRaw = reduceOneExpr testBindIfRaw
testReduceIf :: IO ()
testReduceIf = putStrLn $ oneExpr $ testReduceIfRaw

-- ----- Test FP4.1 ------

testFactor = runParser $ factor

testExpr = runParser $ expr

testTerm = runParser $ term

testCond = runParser $ cond

testFunc = runParser $ func

testOrder = runParser $ order

testCondHelper = runParser $ condHelper