{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

fStar :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
fStar f g (x, y) = (f x, g y)

-- | Exercise 1: implement a functor instance for Parser
instance Functor Parser where
  fmap f (Parser {runParser = g}) = Parser {runParser = fmap (fStar f id) . g}

-- or equivalently,
--first :: (a -> b) -> (a,c) -> (b,c)
--first f (x, y) = (f x, y)

--instance Functor Parser where
--fmap f (Parser {runParser = g}) = Parser {runParser = fmap (first f) . g}


-- | Exercise 2
colim :: Maybe a -> Maybe b -> Maybe (a, b)
colim _ Nothing = Nothing
colim Nothing _ = Nothing
colim (Just x) (Just y) = Just (x, y)

instance Applicative Parser where
  pure x = Parser {runParser = \ s -> Just (x, s)} 
  pF <*> pA = Parser h
    where 
      h s =
        let yo = runParser pF s -- :: Maybe (a -> b, String)
            fcn = fmap fst yo -- :: Maybe (a -> b)
            preValue = fmap snd yo -- :: Maybe String
            midValue = preValue >>= (runParser pA) -- :: Maybe (a, String)
            -- using that Maybe is a Monad
            -- now the (Maybe a) part is the value to which we want to apply fcn
            -- and the (Maybe String) part is the rest of the input string
            -- that comes along for the ride
            value = fmap fst midValue -- :: Maybe a
            rest = fmap snd midValue -- :: Maybe String
            outValue = fcn <*> value -- :: Maybe b
            -- using that Maybe is an Applicative
        in colim outValue rest

-- | Exercise 3

fun2 :: Char -> Char -> (Char, Char)
fun2 x y = (x, y)

fun3 :: Char -> Char -> Char -> (Char, Char, Char)
fun3 x y z = (x, y, z)

abParser :: Parser (Char, Char)
abParser = (pure fun2) <*> (char 'a') <*> (char 'b')

abcParser :: Parser (Char, Char, Char)
abcParser = (pure fun3) <*> (char 'a') <*> (char 'b') <*> (char 'c')
