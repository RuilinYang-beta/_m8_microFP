
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

data Expr = √ Const Integer
          | √ Var String
          | √ Mult Expr Expr
          | √ Add Expr Expr
          | ~If Cond Expr Expr~
          | ~Dec Expr~
          | √ FunCall String Expr
          deriving Show

data Cond = Eq Expr Expr
          deriving Show

data FunDef = FunDef String String Expr
            deriving Show



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












