--reading:
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
	| otherwise = 3*n + 1

steps :: Integer -> Integer
steps =  evener . (`div` 2)

allValues :: Integer -> [Integer]
allValues = takeWhile (>1) . iterate steps . evener

fun2 :: Integer -> Integer
fun2 = foldl' (+) 0 . allValues

--Exercise 2: Folding with trees
--the integer in the Node is the height of the tree at that node
data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a) 
   deriving (Show, Eq)

divide :: [a] -> [[a]]
divide xs = [take n xs, drop n xs]
		where n = length xs `div` 2

split :: [[a]] -> [[a]]
split [xs, ys] = [xs, take 1 ys, drop 1 ys]

getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node h (lTree) n (rTree)) = h

foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree [x, y] = Node 1 (Node 0 Leaf x Leaf) y Leaf
foldTree [x, y, z] = Node 1 (Node 0 Leaf x Leaf) y (Node 0 Leaf z Leaf)
foldTree zs = Node h lTree (head y) rTree
				where 
					[xs, y, ys] = split . divide $ zs
					lTree = foldTree xs
					rTree = foldTree ys
					h = 1 + max (getHeight lTree) (getHeight rTree)


--Exercise 3: More folds!
--(a)
bool2int :: Bool -> Integer
bool2int True = 1
bool2int False = 0

int2bool :: Integer -> Bool
int2bool n
	| even n = False
	| otherwise = True

xor :: [Bool] -> Bool
xor =  int2bool . foldl (+) 0 . map bool2int

--(b)
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


