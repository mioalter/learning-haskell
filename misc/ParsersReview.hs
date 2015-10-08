import           Control.Applicative
import           Data.Char


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- Good on definition
-- Again, what is the difference between type, data, and newtype?
-- type is for type synonyms
-- data is a totally general way of making our own datatypes
-- newtype is specifically for wrapping one type and calling it another
-- so newtypes can have only ONE type constructor which can have only ONE field
-- why? Newtypes are *faster*: Haskell doesn't have to do all the wrapping and unwrapping it does for general ADTs

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

-- PS, inParser :: ((String -> Maybe (a1, String)) -> String -> Maybe (a, String)) -> Parser a1 -> Parser a
inParser f = Parser . f . runParser

-- so f :: (String -> Maybe (a1, String)) -> String -> Maybe (a, String)
-- so we can obviously get f from a function g :: a1 -> a
-- f h s = (first g) <$> h s = (fmap . first $ g) $ h s
-- so we are just saying: let's roll (fmap . first) into the function we are feeding in.
-- this makes clear the first half of the Functor instance of Parser
-- what else is happening? What is the second fmap doing?
-- Given g :: a -> b
-- fmap . first $ g :: Maybe (a, c) -> Maybe (b, c)
-- but we want functions String -> ... so we use the ((->) String) functor's fmap
-- fmap . fmap . first $ g :: (String -> Maybe(a,c)) -> String -> Maybe (b,c)
-- which is exactly what we want to use our inParser function
-- So inParser applied to that gives us a function Parser a -> Parser b.
-- NICE. Holy schnikes, that is so good.


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

-- How do we make Parser and Applicative?
-- pure is pretty clear: given f :: a -> b, (pure f) is the Parser tthat, for any input string s returns Just (f,s)
-- so it wraps up the function and doesn't consume any of the string.
-- Next, <*>? If ((->) r) and Maybe are Applicatives, we should be able to define Parser as one entirely in terms of their aps.
-- (->) r is an Applicative in the way you expect
-- (g :: r -> a -> b) <*> (h :: r -> a ) = \r' -> (g r') (h r')

first' :: (a -> b, c) -> (a,c) -> (b,c)
-- we are assuming we don't care about the second part of the first tuple
first' (f,z) (x,z') = (f x, z')

instance Applicative Parser where
  pure f = Parser {runParser = \ s -> Just (f, s)}
  --f <*> x = Parser{ runParser = \s -> first' <$> (runParser f $ s) <*> (runParser x $ s)}
  (Parser f) <*> x = Parser $ \s -> 
    case f s of 
      Nothing -> Nothing
      Just (g, s') ->  runParser (g <$> x) s'

-- RIGHT! We consumed the s and got an s' in the first step (using Parser f)
-- Now we want to consume s'

phi :: Integer -> Integer
phi x = x^2

squareParse :: Parser Integer
squareParse = pure phi <*> posInt

-- This seems to work correctly.
-- Next, let's compare to Yorgey's instance. We could probably tighten ours up.
-- Ah, he uses Case statements. 
-- Oh, OOPs, we don't want to though out the first String, we want to parse it with f, then proceed!
-- So this DOESN'T do exactly the right thing.
-- Let's change our implementation, then tighten up.









--instance Applicative Parser where
--  pure a = Parser (\s -> Just (a, s))
--  (Parser fp) <*> xp = Parser $ \s ->
--    case fp s of
--      Nothing     -> Nothing
--      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2