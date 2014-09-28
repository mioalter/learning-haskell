{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Array

--Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2))

fibs1 :: [Integer]
fibs1 = map fib [0..]

--Exercise 2

--unnecessary attempts to use arrays when lists are just fine.

--fibsA :: Integer -> Array Integer Integer
--fibsA n = a where 
--		a = array (0,n) ( [(0, 1), (1, 1)] ++ [(i, a!(i-2) + a!(i-1)) | i <- [2..n]] )

--bounds2list :: Array Integer Integer -> [Integer]
--bounds2list a = [fst $ bounds a..snd $ bounds a]

--proj :: Array Integer Integer -> [Integer]
--proj a = [a!i | i <- bounds2list a]

--fibsB :: Integer -> [Integer]
--fibsB n = a where
--		a = [1,1] ++ [a!!(i-2) + a!!(i-1) | i <- [2..n]]

--t = [1,1] ++ [t!!(i-2) + t!!(i-1) | i <- [2..15]]

fibs2 :: [Integer]
fibs2 = t where
	t = [0, 1] ++ [t!!(i-2) + t!!(i-1) | i <- [2..]]

--Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : (streamToList y)

instance Show a => Show (Stream a) where
	show x = show (take 20 $ streamToList x)

--Exercise 4

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

--Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

nonNegs :: Stream Integer
nonNegs = streamFromSeed (+1) 1

log2 :: Integer -> Integer
log2 1 = 0
log2 2 = 1
log2 n 
	| n `div` 2 * 2 == n = 1 + log2 (n `div` 2)
	| otherwise = 0

ruler :: Stream Integer
ruler = streamMap log2 nonNegs

--Exercise 6

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

one :: Stream Integer
one = Cons 1 $ streamRepeat 0

scale :: Integer -> Stream Integer -> Stream Integer
scale n (Cons a b) = Cons (n * a) (scale n b)

--scales by the reciprocal of an integer, only produces a stream of integers
--if all the coefficients actually are divisible by the integer
scaleRecip :: Integer -> Stream Integer -> Stream Integer
scaleRecip b (Cons a a') = Cons (a `div` b) (scaleRecip b a')

instance Num (Stream Integer) where
	fromInteger n = Cons n $ streamRepeat 0
	negate (Cons a b) = Cons ((-1) * a) (negate b)
	(+) (Cons a b) (Cons c d) = Cons (a + c) (b + d)
	(*) (Cons a a') (Cons b b') = Cons (a * b) ((scale a b') + (a' * (Cons b b')))

instance Fractional (Stream Integer) where
	(/) (Cons a a') (Cons b b') = q where
								q = Cons (a `div` b) (scaleRecip b (a' + negate(q * b'))) 

streamFromList :: [a] -> Stream a
streamFromList (x:xs) = Cons x (streamFromList xs)

-- the Fibonacci numbers as a generating function
fibsGF :: Stream Integer
fibsGF = streamFromList fibs2

-- defining the Fibonacci numbers as a generating function F(x) and observing that
-- x = F(x) - xF(x) - x^2 F(x) and solving, we find that
-- F(x) = x / (1 - x - x^2)
-- we may thus directly define F(x) as this ratio of generating functions
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

--Exercise 7
--An O(log n) way to find the nth Fibonacci number
data Matrix a = Square a a a a

instance Show a => Show (Matrix a) where
	show (Square a b c d) = show [a,b,c,d]

instance Num (Matrix Integer) where
	(*) (Square a b c d) (Square a' b' c' d') = Square (a * a' + b * c') (a * b' + b * d') (a' * c + c' * d) (c * b' + d * d')
	(+) (Square a b c d) (Square a' b' c' d') = Square (a + a') (b + b') (c + c') (d + d')
	negate (Square a b c d) = Square ((-1) * a) ((-1) * b) ((-1) * c) ((-1) * d)

proj2 :: Matrix Integer -> Integer
proj2 (Square a b c d) = b

fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = proj2 $ (Square 1 1 1 0) ^ n

