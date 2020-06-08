
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
-- in parser, think about the order of rules: simpler things first

⟨program⟩  ::= ( ⟨function⟩ )+

⟨function⟩ ::= identifier (identifier | integer)* ’:=’ ⟨expr⟩ ’;’

⟨expr⟩     ::= ⟨term⟩ 
             | ⟨term⟩ ( ’+’ | ’-’ ) ⟨expr⟩


⟨term⟩     ::= ⟨factor⟩ 
             | ⟨factor⟩ ’*’ ⟨term⟩


⟨factor⟩   ::= integer
             | identifier
             | identifier ’(’ ⟨expr⟩ (’,’⟨expr⟩) ∗ ’)’
             | ’(’ ⟨expr⟩ ’)’
             | ⟨if⟩


⟨if⟩ ::= ’if’ ’(’ ⟨cond⟩ ’)’ ’then’ ’{’ ⟨expr⟩ ’}’ ’else’ ’{’ ⟨expr⟩ ’}’
⟨cond⟩ ::= ⟨expr⟩ ⟨ordering⟩ ⟨expr⟩
⟨ordering⟩ ::= ’<’|’==’|’>’

-}


data Func = Assign String [Expr] Expr   -- in the last there's a ;

data Expr = Constant   Integer
          | Var        String 
          | Add        Expr Expr
          | Sub        Expr Expr
          | Mul        Expr Expr
          | FunCall    String [Expr]
          | If         Cond Expr Expr
          deriving (Show, Eq)


data Cond = Cond Expr Order Expr
          deriving (Show, Eq)

data Order = Gt | Eq | Lt
           deriving (Eq)


instance Show Order where
    show Gt = ">" 
    show Eq = "=="
    show Lt = "<"












