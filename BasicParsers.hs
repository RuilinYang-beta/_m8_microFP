-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Ruilin Yang (s2099497)
-- Student 2: Joanna Rostek (s2459698)

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
-- parses only the middle input, ignoring the sides
between :: Parser a -> Parser b -> Parser c -> Parser b
between pa pb pc = pure id <*> (pa *> (pb <* pc))

-- skips all whitespace surrounding input
whitespace :: Parser a -> Parser a
whitespace pa = between skip pa skip

-- defines whitespace to skip
skip :: Parser [Char]
skip =  many (char '\n' <|> char '\t' <|> char ' ')


-- ----------FP2.3 ----------

-- parses input (at least one element) separated by a specified separator into a list
sep1 :: Parser a -> Parser b -> Parser [a]
sep1 p s = pure (:) <*> p <*> manyPAndS p s

-- helper functions for parsing the separator, ignoring the result and keeping on parsing elements
manyPAndS p s = pure mconcat <*> many (pAndS p s)      -- use mconcat to flatten [[a]]
pAndS p s = (pure (:[]) <*> s) *> (pure (:[]) <*> p)   -- parse separator first

-- works as sep1, but for parsing 0 or more elements
sep :: Parser a -> Parser b -> Parser [a]
sep p s = sep1 p s <|> pure ([])

-- tries to apply the parser - the 2nd argument, on failure returns the first argument
option :: a -> Parser a -> Parser a
option x pa = P func 
    where func = (\input -> let res1 = runParser pa input in 
                            if length res1 /= 0 then res1 else [(x, input)])

-- ---------- FP2.4 ----------

-- parses a string using char, without skipping whitespace
string :: String -> Parser String 
string []     = pure ([])
string (x:xs) = pure (:) <*> char x <*> string xs 

-- parses an identifier - a letter optionally followed by letters or digits, skips surrounding whitespace
identifier :: Parser String 
identifier = between skip p skip
    where p = pure (:) <*> letter <*> many (letter <|> dig)

-- parses an integer skipping surrounding whitespace
integer :: Parser Integer 
integer = fmap f $ between skip p skip 
    where p = some (dig)
          f = (\input -> read input :: Integer)        

-- parses a string skipping surrounding whitespace
symbol :: String -> Parser String 
symbol str = between skip p skip 
    where p = string str

-- parses input surrounded by parentheses
parens :: Parser a -> Parser a
parens pa = between (symbol "(") pa (symbol ")")

-- parses input surrounded by braces
braces :: Parser a -> Parser a
braces pa = between (symbol "{") pa (symbol "}")


-- ---------- test section ----------

{-
running tests: testSomeFunctionName (Stream "input to parse")
-}

testLetter = runParser $ letter -- will parse a single letter and nothing else

testDigit = runParser $ dig -- will parse a single digit and nothing else

testBetween = runParser $ between (char '(') letter (char ')') -- will parse a letter between parentheses

testSkip = runParser $ skip -- this will by design parse whitespace to skip, not actually skip whitespace!

testWhitespace = runParser $ whitespace letter -- will parse a letter surrounded by whitespace, skipping the whitespace

testSep1 = runParser $ sep1 identifier (symbol "") -- will parse (1+) identifiers separated by spaces

testSep1' = runParser $ sep1 identifier (symbol ",") -- will parse (1+) identifiers separated by commas

testSep = runParser $ sep identifier (symbol "") -- will parse (0+) identifiers separated by spaces

testOption = runParser $ option "not parsed" identifier -- will try to parse an identifier or result in "not parsed"

testIdentifier = runParser $ identifier -- will parse an identifier and nothing else

testInteger = runParser $ integer -- will parse an integer and nothing else

testSymbol = runParser $ symbol "test" -- will parse the symbol "test" and nothing else

testParens = runParser $ parens identifier -- will parse an identifier surrounded by parentheses and nothing else

testBraces = runParser $ braces identifier 








