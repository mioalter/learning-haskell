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
	| even n = n
	| otherwise = 3*n+1

steps :: Integer -> Integer
steps = (`div` 2) . evener

allValues :: Integer -> [Integer]
allValues = takeWhile (>1) . iterate steps

--fun2 :: Integer -> Integer
