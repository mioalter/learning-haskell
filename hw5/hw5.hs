import ExprT
import Parser
import Data.Maybe

--Exercise 1
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)

--Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

--Exercise 3
class Expr a where
	lit :: Integer -> a
	add :: a -> a -> a
	mul:: a -> a -> a

instance Expr ExprT where
	lit = Lit
	add = Add
	mul = Mul

--Exercise 4
--newtype ExprBool = 
--instance Expr ExprBool where
--	lit = Lit
--	add (lit a) (lit b) = (lit a) || (lit b)
--	mul (lit a) (lit b) = (lit a) && (lit b)