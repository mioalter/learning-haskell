--iterate :: (a -> a) -> a -> [a] returns [x,f(x), f(f(x)), ...]
--takeWhile :: (a -> Bool) -> [a] -> [a] takes from a list until a condition fails to be met

import Data.List

--Exercise 1: Wholemeal Programming
--(a)
fun1 :: [Integer] -> Integer
fun1 =  foldl' (*) 1 . map (+ (-2)) . filter even

--(b)
evener :: Integer -> Integer
evener n
	| n <= 1 = 0
	| even n = n
	| otherwise = 3*n+1

steps :: Integer -> Integer
steps =  evener . (`div` 2)

allValues :: Integer -> [Integer]
allValues = takeWhile (>1) . iterate steps . evener

fun2 :: Integer -> Integer
fun2 = foldl' (+) 0 . allValues