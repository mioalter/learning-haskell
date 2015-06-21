{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Maybe
import Data.Char


------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

conser :: a -> [a] -> [a]
conser x xs = x:xs

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = Parser f
	where 
		f [] = Just([],[])
		f x 
			| isJust $ (runParser p) x = (runParser $ oneOrMore p) x
			| otherwise = Just ([], x)

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (pure conser) <*> p <*> zeroOrMore p


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = (pure conser) <*> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

inter :: Integer -> Atom
inter i = N i

identer :: Ident -> Atom
identer i = I i 

atomParser :: Parser Atom
atomParser = ((pure inter) <*> posInt) <|> ((pure identer) <*> ident)

atomParserPlus :: Parser Atom
atomParserPlus = spaces *> atomParser <* spaces

atomListParser :: Parser [Atom]
atomListParser = zeroOrMore $ atomParserPlus

j1 :: Atom -> SExpr
j1 a = A a

--j1Plus :: Parser Atom -> Parser SExpr
--j1Plus = fmap j1

j2 :: [SExpr] -> SExpr
j2 l = Comb l

j3 :: [Atom] -> SExpr
j3 xs =  j2 $ map j1 xs


openParser :: Parser Char
openParser = char '('

closeParser :: Parser Char
closeParser = char ')'

sexprParser :: Parser SExpr
sexprParser = ((pure j1) <*> atomParserPlus) <|> ((pure j2) <*> sexprListParser)

sexprListParser :: Parser [SExpr]
sexprListParser = openParser *> spaces *>(zeroOrMore sexprParser) <* spaces <* closeParser


{-
Ideas:
* make sexprListParser :: Parser [SExpr] using zeroOrMore
* our sexprParser :: Parser SExpr should be 
((pure j1) <*> atomParserPlus) <|> ((pure j2) <*> sexprListParser)
* we need to define sexprListParser to work on things that start with '(' end with ')'
atomParserPlus will fail on those expressions and they will get passed to sexprListParser
* the recursion will be: sexprList parser is defined in terms of sexprParser and sexprParser is defined in terms of sexprListParser
* we don't need anything of type Parser [Atom]
-}


