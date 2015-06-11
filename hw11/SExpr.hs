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
		f [] = Nothing
		f x 
			| isJust $ (runParser p) x = (runParser $ oneOrMore p) x
			| otherwise = Just ([], x)

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (pure conser) <*> p <*> zeroOrMore p


------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = undefined

ident :: Parser String
ident = undefined

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
