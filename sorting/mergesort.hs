{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Array
import Data.Monoid

-- | Merge Sort I: Lists
mergeSortedList :: ([Int],[Int]) -> [Int]
mergeSortedList ([],[]) = []
mergeSortedList (xs,[]) = xs
mergeSortedList ([],ys) = ys
mergeSortedList ((x:xs),(y:ys)) 
	| x <= y = x : mergeSortedList (xs, (y:ys))
	| otherwise = y : mergeSortedList ((x:xs), ys)

-- this is super inefficient because 
-- you have to step through the whole list to divide it in half
-- we should definitely use an *array*, not a list
splitList :: [Int] -> ([Int],[Int])
splitList x = (take n x, drop n x)
	where n = fromIntegral $ length(x) `div` 2

mergeSortList :: [Int] -> [Int]
mergeSortList [] = []
mergeSortList [x] = [x]
mergeSortList xs = mergeSortedList (mergeSortList a, mergeSortList b)
	where (a,b) = splitList xs

{-
Run-time analysis
Let T(n) be the cost (in time) of sorting an array of length n
then

T(n) = 2T(n/2) + n

that is, sorting one list can be done by 
* splitting the list into two sublists of length n/2 and solving the problem on each of those: this costs 2 T(n/2), 
* then merging those two sorted lists into a single sorted list: this costs n
that merging costs n is obvious from the mergeSortedList function: each comparison that you do removes one element from consideration
leaving n-1 total elements to handle.

We use this recursive relationship to solve for T(n)

T(n) = 2T(n/2) + n
	 = 2 [2 T(n/4) + n/2] + n 
	 = 4 T(n/4) + 2n
	 ...
	 = 2^(log_2 n) T(1) + n log_2 n
	 = n + n log n

Now, remember that O(f(n)) is the set of all functions g(n) for which there exists a constant C such that g(n) <= Cf(n)

Claim (n + n log n) is in O(n log n).
Proof: Let C = 2, then for n >= 2
1 <= log n
1 <= (C-1) log n
n <= (C-1) n log n
n <= C n log n - (n log n)
n + n log n <= C n log n 
QED.

Therefore Merge Sort is O(n log n).
This is a special case of the Master Theorem, but it is obviously easy to prove it directly.
-}


{- 
Now, let re-implement MergeSort using arrays.
Arrays contain there bounds as data so, in particular, we know how long an array is without having to step through it one element at a time 
-}


-- | Merge Sort II: Arrays (aborted)
g = array (1,10) [(i,i^2) | i<- [1..10]] :: Array Int Int

lenArray :: Array Int Int -> Int
lenArray as = snd . bounds $ as

lenFront :: Array Int Int -> Int
lenFront as = fromIntegral $ lenArray as `div` 2

lenBack :: Array Int Int -> Int
lenBack as = (lenArray as) - (lenFront as) 

frontArray :: Int -> Array Int Int -> Array Int Int
frontArray n as = listArray (1,n) (take n $ elems as)

backArray :: Int -> Array Int Int -> Array Int Int
backArray n as = listArray (1,n) (drop n $ elems as)

splitArray :: Array Int Int -> (Array Int Int, Array Int Int)
splitArray as = (frontArray (lenFront as) as, backArray (lenBack as) as)


{- ABORT!

Two problems:
(1) You cannot have an empty array so splitting arrays in "half" won't work.
Reason: an array is a function from indices to values. It is impossible to define a function on the empty set.
(2) This is overkill: we don't need random access to array elements, we just wanted a list that knows its own length
Elaboration: since the values of an array are stored in a list, using arrays doesn't avoid stepping through the list with 
the (take n) and (drop n) functions to split the values of the array in two, 
the only thing we get from an array is the bounds which tells us the length

Solution: let's just define a new data type that is a list which knows its own length.

-}

-- | Merge Sort III: LenList, the list that knows its own length

data LenList a = LenList {len :: Int, vals :: [a]}
	deriving(Eq, Show)

consL :: a -> LenList a -> LenList a
consL x (LenList n xs) = LenList (n+1) (x:xs)

takeL :: Int -> LenList a -> LenList a
takeL n (LenList l xs) = LenList n (take n xs)

dropL :: Int -> LenList a -> LenList a
dropL n (LenList l xs) = LenList (l-n) (drop n xs)

reverseL :: LenList a -> LenList a
reverseL (LenList n xs) = LenList n (reverse xs)


instance Monoid (LenList a) where
	mempty = LenList 0 []
	mappend (LenList n xs) (LenList m ys) = LenList (n+m) (xs ++ ys)

-- Note: takeL 5 b == LenList 0 []
-- so LenLists, consL, takeL, dropL have no trouble with the empty list.

-- some LenLists for testing
a = LenList 0 []
b = LenList 5 [1..5] :: LenList Int
c = LenList 6 [11..16] :: LenList Int
w = reverseL b
z = reverseL c
x = mappend w z

mergeSorted :: (LenList Int, LenList Int) -> LenList Int
mergeSorted (LenList 0 [], LenList 0 []) = LenList 0 []
mergeSorted (LenList n xs, LenList 0 []) = LenList n xs
mergeSorted (LenList 0 [], LenList m ys) = LenList m ys
mergeSorted (LenList n (x:xs), LenList m (y:ys))
	|  x <= y =  consL x $ mergeSorted (LenList (n-1) xs, LenList m (y:ys))
	| otherwise = consL y $ mergeSorted (LenList n (x:xs), LenList (m-1) ys)

splitL :: LenList Int -> (LenList Int, LenList Int)
splitL lxs = (takeL k lxs, dropL k lxs)
	where k = fromIntegral $ len lxs `div` 2

-- there must already be a way to do this....this is the tuple version of 
-- zipWith ($) [fcns] [vals]
-- but here we don't want lists of arbitrary length, we want pairs
applyL :: (a -> b, c -> d) -> (a, c) -> (b, d)
applyL (f, g) (x,y) = (f x, g y)

diagonal :: a -> (a,a)
diagonal x = (x,x)

-- Now it's extremely obvious what we're doing
-- we take a list, split it, mergeSort each half, then mergeSorted the two (sorted) halves.
mergeSort :: LenList Int -> LenList Int
mergeSort (LenList 0 []) = LenList 0 []
mergeSort (LenList 1 [x]) = LenList 1 [x]
mergeSort lxs = mergeSorted . dMerge . splitL $ lxs
	where dMerge = applyL $ diagonal mergeSort

-- Note this stuff with the applyL and diagonal functions might look fancy, but it's not:
-- dMerge (left, right) 
-- is just 
-- (mergeSort left, mergeSort right)
-- so 
-- mergeSorted . dMerge . splitL $ lxs
-- is just
-- mergeSorted (mergeSort left, mergeSort right)
-- where (left,right) = splitL lsx

{-
Two questions:
(1) Did it make sense to define LenList like this, or should we have defined it by analogy with Lists
LenList a = NilL | ConsL a (LenList a)
Does it matter? 
(2) Was it actually worth defining a new data structure and having to write LenList everywhere?
-}




