module AParser where

import           Control.Applicative
import           Data.Char

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

{- This is Yorgey's slicker implementation than mine.
Two ideas: 
I.
* using Maybe's fmap, fmap . first $ f :: Maybe (a,c) -> Maybe (b,c)
* then using (->) String's fmap
fmap . fmap . first $ f :: (String -> Maybe (a,c)) -> (String -> Maybe (b,c))
It looks like these are the two fmaps we are using, the first for the Maybe functor, the second for the Reader functor
II. We can use typed holes to get a better idea (https://wiki.haskell.org/GHC/Typed_holes), but we'll have to update GHC (>=7.8.1)
-}

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2