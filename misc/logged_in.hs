
-- Let's assume the accumulator intervals are ordered
{- 
ideas: 
* define (Int,Int) as an instance of Ord and use the comparison to write a funtion that combines two intervals into a list of intervals
* to do this, we need to define Bool as an instance of Monoid so do that, too
Nope, don't need to do that: the function and is all I need
and :: [Bool] -> Bool
* oh, looks like Ord won't work because we also want to allow for one interval
to be contained in or contain another
(1) getting illegal instance error
(2) it does look like I want to make a data type with values LT, LEQ, GT, GEQ, Contains, isContained
and use that
* we will compare userInterval over the accumulator (a list of intervals) to get a list of Compares
then decide what to return based on that
No, that's not quite right: we'll want to fold using a function that takes two intervals and updates the
accumulator based on the value of compare on those two intervals
-}

{- 

Statement of problem: 
An interval is a tuple of integers (a,b) representing an interval of time for which a user was logged in
Given a accumulator of ordered, disjoint, intervals representing the total time a user has been logged in
Write a function that takes a new interval (representing another interval of user time) and updates the accumulator.
The new interval can overlap, be contained in, or contain multiple of the intervals in the accumulator.

The idea is that a user could be logged in on multiple devices and we want to measure the total time logged in across all devices.

-}

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Monoid

-- note: we could make Interval a functor Ord a => Interval a = (a, a)
-- we really also should check that the endpoint is >= starting point and use Maybe
-- so we'd write a "load new interval" function :: (Int, Int) -> Maybe Interval that returns Nothing if end < start and Just (start, end) otherwise.

type Interval = (Integer, Integer)

data Compare = L | LEQ | G | GEQ | Contains | IsContained
	deriving (Eq, Show)

class Containable a where
	l :: a -> a -> Bool
	leq :: a -> a -> Bool
	geq :: a -> a -> Bool
	g :: a -> a -> Bool
	contains :: a -> a -> Bool
	isContained :: a -> a -> Bool

-- optimization: where a = (a_0,a_1) and b = (b_0, b_1)
instance Containable Interval where
	l a b = snd a < fst b
	leq a b = and [fst a <= fst b, snd a >= fst b, snd a <= snd b]
	geq a b = and [fst a >= fst b, fst a <= snd b, snd a >= snd b]
	g a b = fst a > snd b
	contains a b = and [fst a <= fst b, snd a >= snd b]
	isContained a b = contains b a


compareInterval :: Interval -> Interval -> Compare
compareInterval a b
	| l a b = L
	| leq a b = LEQ
	| geq a b = GEQ
	| g a b = G
	| contains a b = Contains
	| isContained a b = IsContained

-- Great, now I can map (compare new_interval) over a list of intervals and get a list of Compares.
-- The question is: how do I use this. Should I do a binary search? I just want to find the 
-- one or more intervals that are not L or G and combine the new interval with them

-- Now write a function that traverses the list and returns the sublist of intervals that do not evaluate to L or G.
-- We are going to do insertion and deletion in our accumulator
-- so maybe we want to return three lists: all the L intervals, all the G intervals, all the not-L-or-G intervals
-- we're write a separate function that does the combining and we'll call it on the not-L-or-G intervals, then concat the three.


--our function which takes a new interval and an ordered list of known intervals
-- and returns the list of Ls, the list of not L or Gs, and the list of Gs
-- conflict: [[Interval], [Interval], [Interval]] is actually just [[Interval]]
-- we want exactly three lists, but if we use tuples, we get a type mismatch
-- because foldl works with lists

c = [(1,4), (8,12), (15, 20), (24, 30), (33,38), (41, 47)]

-- optimization: do this with one pass over the list, not three
triage :: [(Interval, Compare)] -> ([(Interval, Compare)], [(Interval, Compare)],[(Interval, Compare)])
triage xs = (filter (\x -> snd x == G) xs, filter (\x -> snd x /= G && snd x /= L) xs, filter (\x -> snd x == L) xs)

-- optimization: doing map and then zip makes two passes; do this with a single fold
partition :: Interval -> [Interval] -> ([(Interval, Compare)], [(Interval, Compare)],[(Interval, Compare)])
partition x ys = triage $ zip ys (map (compareInterval x) ys)

-- optimization: can we consolidate some of these patterns?
absorb :: Interval -> [(Interval, Compare)] -> [Interval]
absorb x [(y, LEQ)] = [(fst x, snd y)]
absorb x [(y, GEQ)] = [(fst y, snd x)]
absorb x [(y, IsContained)] = [y]
absorb x [(y, Contains)] = [x]
absorb x ((y, Contains):ys)
	| c == Contains = [x]
	| c == LEQ = [(fst x, snd z)]
	where (z, c) = head . reverse $ ys
absorb x ((y, GEQ):ys)
	| d == Contains = [(fst y, snd x)] 
	| d == LEQ = [(fst y, snd w)]
	where (w, d) = head . reverse $ ys
absorb x [] = [x]
absorb x ys = []

-- is there a more elegant way to write this?
-- how do you take a tuple of functions (f, g, h) and apply them componentwise to a tuple of values (a, b, c)?
update :: Interval -> [Interval] -> [Interval]
update x ys = (map fst a) ++ (absorb x b) ++ (map fst c)
	where (a, b, c) = partition x ys
