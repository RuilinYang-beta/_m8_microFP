
{-
⟨program⟩  ::= ( ⟨function⟩ )+
⟨function⟩ ::= identifier (identifier | integer)* ’:=’ ⟨expr⟩ ’;’
⟨expr⟩     ::= ⟨term⟩ | ⟨term⟩ ( ’+’ | ’-’ ) ⟨expr⟩
⟨term⟩     ::= ⟨factor⟩ | ⟨factor⟩ ’*’ ⟨term⟩
⟨factor⟩   ::= integer
             | identifier ( ’(’ ⟨expr⟩ (’,’⟨expr⟩) ∗ ’)’)?
             | ’if’ ’(’ ⟨expr⟩ ⟨ordering⟩ ⟨expr⟩ ’)’ ’then’ ’{’ ⟨expr⟩ ’}’ ’else’ ’{’ ⟨expr⟩ ’}’
             | ’(’ ⟨expr⟩ ’)’

⟨ordering⟩ ::= ’<’|’==’|’>’
-}


{-
-- grammar rewritten

⟨program⟩  ::= ( ⟨function⟩ )+

⟨function⟩ ::= identifier (identifier | integer)* ’:=’ ⟨expr⟩ ’;’

⟨expr⟩     ::= ⟨term⟩ 
             | ⟨term⟩ ( ’+’ | ’-’ ) ⟨expr⟩

⟨term⟩     ::= ⟨factor⟩ 
             | ⟨factor⟩ ’*’ ⟨term⟩

⟨factor⟩   ::= integer
             | identifier
             | identifier ’(’ ⟨exprs⟩ ’)’
             | ’if’ ’(’ ⟨expr⟩ ⟨ordering⟩ ⟨expr⟩ ’)’ ’then’ ’{’ ⟨expr⟩ ’}’ ’else’ ’{’ ⟨expr⟩ ’}’
             | ’(’ ⟨expr⟩ ’)’

⟨exprs⟩    ::= ⟨expr⟩ 
             | ⟨expr⟩ ’,’ ⟨exprs⟩


⟨ordering⟩ ::= ’<’|’==’|’>’

-}

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











