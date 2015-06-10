module AParser where

import           Control.Applicative

import           Data.Char
import Data.Maybe

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

posInt' :: Parser Int
posInt' = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, drop 1 rest)
      where (ns, rest) = span isDigit xs

fStar :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
fStar f g (x, y) = (f x, g y)

colim :: Maybe a -> Maybe b -> Maybe (a, b)
colim (Just x) (Just y) = Just (x, y)
colim _ _ = Nothing

instance Functor Parser where
  fmap f (Parser {runParser = g}) = Parser {runParser = fmap (fStar f id) . g}

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
