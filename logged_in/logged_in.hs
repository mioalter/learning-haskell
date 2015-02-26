{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- A user might be logged in at different times on multiple devices and these sessions might even overlap in various ways.
Given a standardized list of sessions (a list of ordered, disjoint time intervals), 
write a function that takes a new session and updates the list.
That is, it manages the fact that the new session may contain, be contained in, overlap, or all of the above, 
existing sessions and produces a new standardized list of sessions (a list of intervals which are again ordered and disjoint). -}

{- To Do
* optimize functions where indicated
* make a "new session loading" function of type Interval -> Maybe Interval with
loadInterval (a,b) = Just (a,b) if a <= b, Nothing otherwise
* adapt update function to work with it and use that Maybe is a monad to put them together.
* Containable should probably have a type constraint: Ord a => Containable a
-}

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
-- how to do this: (1) explicitly pattern match (2) let... at the beginning (3) where...at the end?
-- can you use let and where in an instance declaration where you are defining multiple functions?
-- Ask on Haskell-Beginners
-- Q2: is it *not* wasteful to write fst a, snd b every where because once Haskell evaluates
-- fst a, the value is there and it doesn' have to evaluate it again the next time you call it?
-- Not an issue: these are different functions, not different patterns of the same function.
-- Haskell is not repeatedly evaluating anything.
instance Containable Interval where
	l (a0,a1) (b0,b1) = a1 < b0
	leq (a0,a1) (b0,b1) = and [a0 <= b0, a1 >= b0, a1 <= b1]
	geq (a0,a1) (b0,b1) = and [a0 >= b0, a0 <= b1, a1 >= b1]
	g (a0,a1) (b0,b1) = a0 > b1
	contains (a0,a1) (b0,b1) = and [a0 <= b0, a1 >= b1]
	isContained a b = contains b a

compareInterval :: Interval -> Interval -> Compare
compareInterval a b
	| l a b = L
	| leq a b = LEQ
	| geq a b = GEQ
	| g a b = G
	| contains a b = Contains
	| isContained a b = IsContained

c = [(1,4), (8,12), (15, 20), (24, 30), (33,38), (41, 47)]

-- optimization: do this with one pass over the list, not three
triage :: [(Interval, Compare)] -> ([(Interval, Compare)], [(Interval, Compare)],[(Interval, Compare)])
triage xs = (filter (\x -> snd x == G) xs, filter (\x -> snd x /= G && snd x /= L) xs, filter (\x -> snd x == L) xs)

-- optimization: doing map and then zip makes two passes; do this with a single fold
partition :: Interval -> [Interval] -> ([(Interval, Compare)], [(Interval, Compare)],[(Interval, Compare)])
partition x ys = triage $ zip ys (map (compareInterval x) ys)

-- optimization: can we consolidate some of these patterns?
-- Also, get the last element without reversing the list. That's costly.
-- Do we want to use an array instead of a list? 
-- Maybe it doesn't matter, but computing the length of a list also requires stepping through it.
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

