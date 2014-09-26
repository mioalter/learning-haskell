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
	t = [1,1] ++ [t!!(i-2) + t!!(i-1) | i <- [2..]]

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