-- Add your names and student numbers to the following file. Do not change anything else, since it is parsed.
-- Student 1: Your Name (sxxxxxxx)
-- Student 2: Other Name (syyyyyyy)

module PComb where
import Control.Applicative
import Data.Char
import Data.Monoid
import Test.QuickCheck

-- Stream of Chars - can be extended with the location for error handling
data Stream = Stream [Char]
              deriving (Eq, Show)

-- ---------- FP1.1 ----------
data Parser r = P {
    runParser :: Stream -> [(r, Stream)]
}


-- ---------- FP1.2 FP1.5 FP1.6----------
instance Functor Parser where
    fmap func parsera = P (\input -> [ (func r, rest) | (r, rest) <- runParser parsera input])


instance Applicative Parser where
    -- pure :: a -> f a
    pure sth = P (\input -> [(sth, input)])
    -- <*> :: f (a -> b) -> f a -> f b
    parsera <*> parserb = P func 
        where func = (\input ->  [(f r, rem')| (f, rem) <- runParser parsera input, 
                                               (r, rem') <- runParser parserb rem])


-- ???????????? better way to write <|> 
instance Alternative Parser where 
    -- empty :: f a
    empty = failure
    -- (<|>) :: fa -> fa -> fa
    parsera <|> parserb = P func 
        where func = (\input ->  let res1 = runParser parsera input in 
                                 if length res1 /= 0 then res1 else runParser parserb input)


-- ---------- FP1.3 ----------
char :: Char -> Parser Char
char c = P func 
       where func (Stream [])      = []
             func (Stream (x:xs))  | x == c    = [(c, (Stream xs))]
                                   | otherwise = []

-- ---------- FP1.4 ----------
failure :: Parser a 
failure = P func
        where func = (\_ -> [])




-- ---------- temp section ----------

p1 = char '1'
p2 = char '2'












