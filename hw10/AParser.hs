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

-- Now, 
-- Parser if a functor
-- on objects: a |-> Parser a = (String -> Maybe (a, String))
-- on maps: (a -> b) -> Parser a -> Parser b
-- to define it as an instance of applicative, need
-- <*> :: f (a -> b) -> f a -> f b
-- where f is Parser
-- Q: how do we even pattern match on a thing of type (Parser (a -> b)) ? Apparenlty (Parser f) is not it.


-- something doesn't quite work, but this is the right idea
instance Applicative Parser where
  pure x = Parser {runParser = \ _ -> Just (x, "")} 
  pF <*> pA = Parser {runParser g}
    where 
      g s
        | yo == Nothing = Nothing
        | otherwise = fmap (fStar function id) value
        where yo = runParser pF s
              function = fst . fromJust $ yo -- :: a -> b
              value = (runParser pA) . snd . fromJust $ yo -- :: Maybe (a, String)

