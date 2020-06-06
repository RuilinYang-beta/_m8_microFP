-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Your Name (sxxxxxxx)
-- Student 2: Other Name (syyyyyyy)

module BasicParsers where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck
import PComb


-- ---------- FP2.1 ----------
-- parses any (alphabetical) letter
letter :: Parser Char 
letter = helper l

-- parses any digit
dig :: Parser Char 
dig = helper d


l = ['a'..'z'] ++ ['A'..'Z']
d = ['0'..'9']
ld = l ++ d

helper :: [Char] -> Parser Char 
helper []      = failure
helper (x:xs)  = char x <|> helper xs 


-- ---------- FP2.2 ----------
between :: Parser a -> Parser b -> Parser c -> Parser b
between pa pb pc = pure id <*> (pa *> (pb <* pc))


whitespace :: Parser a -> Parser a
whitespace pa = between skip pa skip


skip :: Parser [Char]
skip =  many (char '\n' <|> char '\t' <|> char ' ')


-- ----------FP2.3 ----------

-- !!!!!!!!!!!!!!!!
-- future work may needed 
-- currently testSep1 (Stream "a,b,c,d,e") yields [("abcde",Stream "")]
-- there's nothing to seperate a b c d e in the parsing result
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = pure (:) <*> p  <*>  manyPAndS p s


manyPAndS p s = pure mconcat <*> many (pAndS p s)      -- use mconcat to flatten [[a]]
pAndS p s = (pure (:[]) <*> s ) *> (pure (:[]) <*> p)  -- parse separator first


sep :: Parser a -> Parser b -> Parser [a] 
sep p s = sep1 p s <|> pure ([])


option :: a -> Parser a -> Parser a
option x pa = P func 
    where func = (\input -> let res1 = runParser pa input in 
                            if length res1 /= 0 then res1 else [(x, input)])

-- ---------- FP2.4 ----------

string :: String -> Parser String 
string []     = pure ([])
string (x:xs) = pure (:) <*> char x <*> string xs 


identifier :: Parser String 
identifier = between skip p skip 
    where p = some (letter <|> dig)


integer :: Parser Integer 
integer = fmap f $ between skip p skip 
    where p = some (dig)
          f = (\input -> read input :: Integer)        

symbol :: String -> Parser String 
symbol str = between skip p skip 
    where p = string str

parens :: Parser a -> Parser a 
parens pa = between (char '(') pa (char ')')


braces :: Parser a -> Parser a 
braces pa = between (char '{') pa (char '}')


-- ---------- temp section ----------

-- supply the following with (Stream something)

testBetween = runParser $ between (char '(') letter (char ')')

testSkip = runParser $ skip

testWhitespace = runParser $ whitespace letter

testSep1 = runParser $ sep1 letter (char ',')

testSep = runParser $ sep letter (char ',') 

testIdentifier = runParser $ identifier

testInteger = runParser $ integer 

testSymbol = runParser $ symbol "test" 

testParens = runParser $ parens identifier

testBraces = runParser $ braces identifier 








