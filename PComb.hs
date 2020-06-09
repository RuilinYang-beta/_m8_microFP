-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Ruilin Yang (s2099497)
-- Student 2: Joanna Rostek (s2459698)


-- ===========================================================================================
-- Note: about test
-- In order to provide comprehensive test cases without making the code section unreadable, 
-- the tests are provided at the end of `BasicParsers.hs`, `MicroFP.hs` and marked with FPx.y
-- ===========================================================================================


module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

-- ---------- FP1.1 ----------
{-
data Parser - describes the data type and a function runParser, which transforms
an input stream into a series of tokens paired with the remaining stream
-}
data Parser r = P {
    runParser :: Stream -> [(r, Stream)]
}

-- ---------- FP1.2 ----------
{-
Parser is an instance of Functor - fmap applies a function onto the tokenized input
-}
instance Functor Parser where
    fmap func parser_a = P (\input -> [ (func r, rest) | (r, rest) <- runParser parser_a input])

-- ---------- FP1.3 ----------
{-
char - parses a single character
-}
char :: Char -> Parser Char
char c = P func
       where func (Stream [])      = []
             func (Stream (x:xs))  | x == c    = [(c, (Stream xs))]
                                   | otherwise = []

-- ---------- FP1.4 ----------
{-
failure - returns an empty list denoting a failure in parsing
-}
failure :: Parser a
failure = P func
        where func = (\_ -> [])


-- ---------- FP1.5 ----------
{-
Parser is an instance of Applicative:
    pure sth - puts something into the context of Parser
    p_a <*> p_b - combines two parsers
-}
instance Applicative Parser where
    -- pure :: a -> f a
    pure sth = P (\input -> [(sth, input)])
    -- <*> :: f (a -> b) -> f a -> f b
    parser_a <*> parser_b = P func
        where func = (\input ->  [(f r, rem')| (f, rem) <- runParser parser_a input,
                                               (r, rem') <- runParser parser_b rem])


-- ---------- FP1.6 ----------
{-
Parser is an instance of Alternative:
    empty - results in failure
    p_a <|> p_b - tries out parser p_a, if that fails it tries p_b
-}
instance Alternative Parser where 
    -- empty :: f a
    empty = failure
    -- (<|>) :: fa -> fa -> fa
    parser_a <|> parser_b = P func
        where func = (\input ->  let res1 = runParser parser_a input in
                                 if length res1 /= 0 then res1 else runParser parser_b input)













