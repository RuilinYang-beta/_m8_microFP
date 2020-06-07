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

data Func = Assign String [Expr] Expr  deriving Show -- in the last there's a ;

data Expr = Constant   Integer
          | Var        String 
          | Add        Expr Expr
          | Sub        Expr Expr
          | Mul        Expr Expr
          | FunCall    String [Expr]
          | If         Cond Expr Expr
          deriving Show


data Cond = Cond Order Expr Expr
          deriving Show 


data Order = Gt | Eq | Lt
           deriving Show 


-- ---------- FP3.2 ----------

fibonacci = Assign "fibonacci" [(Var "n")] (Add (FunCall "fibonacci" [(Sub (Var "n") (Constant 1))]) (FunCall "fibonacci" [(Sub (Var "n") (Constant 2))]) )

fib = Assign "fib" [(Var "n")] (If (Cond Lt (Var "n") (Constant 3)) (Constant 1) (Add (FunCall "fib" [(Sub (Var "n") (Constant 1))]) (FunCall "fib" [(Sub (Var "n") (Constant 2))])))

sum = Assign "sum" [(Var "a")] (Add (FunCall "sum" [(Sub (Var "a") (Constant 1))]) (Var "a"))

div = Assign "div" [(Var "x"), (Var "y")] (If (Cond Lt (Var "x") (Var "y")) (Constant 0) (Add (Constant 1) (FunCall "div" [(Sub (Var "x") (Var "y")), (Var "y")])))

twice = Assign "twice" [(Var "f"), (Var "x")] (FunCall "f" [(FunCall "f" [(Var "x")])])

add = Assign "add" [(Var "x"), (Var "y")] (Add (Var "x") (Var "y"))

inc = Assign "inc" [] (FunCall "add" [(Constant 1)])

eleven = Assign "eleven" [] (FunCall "inc" [(Constant 10)])


-- ---------- FP3.3 ----------


-- ---------- FP3.4 ----------






-- ---------- FP4.1 ----------

term :: Parser Expr
term = pure Mul <*> (expr <* (symbol "*")) <*> expr
   <|> factor

expr :: Parser Expr
expr = pure Sub <*> (term <* (symbol "-")) <*> expr
   <|> pure Add <*> (term <* (symbol "+")) <*> expr
   <|> term

cond :: Parser Cond
cond = pure Cond <*> pure Gt <*> (expr <* (symbol ">")) <*> expr
  -- <|> pure Cond <*> pure Lt <*> expr <* (symbol "<") <*> expr
  -- <|> pure Cond <*> pure Eq <*> expr <* (symbol "==") <*> expr

factor :: Parser Expr
factor = parens expr
     <|> pure If <*> (between (symbol "if") cond (symbol "then")) <*> expr <* (symbol "else") <*> expr
     <|> pure Var <*> identifier
     <|> pure Constant <*> integer

func :: Parser Func
func = pure Assign <*> identifier <*> sep1 idOrInt (symbol ",") <* (symbol ":=") <*> expr <* (symbol ";")

idOrInt = pure Var <*> identifier
      <|> pure Constant <*> integer

-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll

-- --------- temp section -----------

-- supply the following with (Stream something)

testFactor = runParser $ factor

testExpr = runParser $ expr

testTerm = runParser $ term

testCond = runParser $ cond

testFunc = runParser $ func









