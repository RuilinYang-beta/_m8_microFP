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

data Func = Assign String [Expr] Expr

data Expr = Const      Integer
		  | Var        String 
		  | Add        Expr Expr
		  | Sub        Expr Expr
		  | Mul        Expr Expr
		  | FunCall    String [Expr]
		  | If         Cond Expr Expr
		  deriving Show


data Cond = Cond Ord Expr Expr
          deriving Show 


data Ord = Gt | Eq | Lt
         deriving Show 



-- QuickCheck: all prop_* tests
return []
check = $quickCheckAll
