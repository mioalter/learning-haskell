{-# LANGUAGE TypeSynonymInstances #-}
import Parser
import Data.Maybe
import qualified Data.Map as M
import Safe

class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul:: a -> a -> a


--Exercise 4
newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

minmax :: MinMax -> Integer
minmax (MinMax n) = n

mod7 :: Mod7 -> Integer
mod7 (Mod7 n) = n `mod` 7

instance Expr Integer where
	lit = id
	add n m = n + m
	mul n m = n * m

instance Expr Bool where
	lit n
		| n <= 0 = False
		| otherwise = True
	add p q = p || q
	mul p q = p && q

instance Expr MinMax where
	lit n = MinMax n
	add p q = MinMax (max (minmax p) (minmax q))
	mul p q = MinMax (min (minmax p) (minmax q))

instance Expr Mod7 where
	lit n = Mod7 n
	add p q = Mod7 ((mod7 p) + (mod7 q) `mod` 7)
	mul p q = Mod7 ((mod7 p) * (mod7 q) `mod` 7)


--for testing
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7


--Exercise 6
class HasVars a where
	var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
	lit = Lit
	add = Add
	mul = Mul

instance HasVars VarExprT where
	var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where

instance Expr (M.Map String Integer -> Maybe Integer) where



