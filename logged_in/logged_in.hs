{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

{- Facebook records all of a user's sessions, the intervals of time for which they are logged in, and likes to know the total time
for which a user is logged in. A wrinkle is that users log in from multiple devices and sessions can overlap in various ways.
Given a standardized list of sessions (a list of ordered, disjoint time intervals), 
write a function that takes a new session and updates the list.
That is, the update function manages the fact that the new session may contain, be contained in, overlap, or all of the above, 
existing sessions and produces a new standardized list of sessions (a list of intervals which are again ordered and disjoint). -}

{- To Do
* optimize functions where indicated
* make a "new session loading" function of type Interval -> Maybe Interval with
loadInterval (a,b) = Just (a,b) if a <= b, Nothing otherwise
* adapt update function to work with it and use that Maybe is a monad to put them together.
* Containable should probably have a type constraint: Ord a => Containable a
That's not quite right: we should define 
data Ord a => Interval a = (a, a)
then we can say Ord a => Containable a
-}

type Interval a = (a, a)

-- Question: this type declaration works, but we have to put Ord a => everywhere else.
-- If we defined a new data type, we could put the Ord a => into the definition, right?
-- Then we would have to give a constructor and write that everywhere, but that might be better.
-- e.g.
-- data Ord a => Interval a = Interval (a,a)
-- or maybe that's not legal, let's look it up.

data Compare = L | LEQ | G | GEQ | Contains | IsContained
	deriving (Eq, Show)

class Containable a where
	l :: a -> a -> Bool
	leq :: a -> a -> Bool
	geq :: a -> a -> Bool
	g :: a -> a -> Bool
	contains :: a -> a -> Bool
	isContained :: a -> a -> Bool

instance Ord a => Containable (Interval a) where
	l (a0,a1) (b0,b1) = a1 < b0
	leq (a0,a1) (b0,b1) = and [a0 <= b0, a1 >= b0, a1 <= b1]
	geq (a0,a1) (b0,b1) = and [a0 >= b0, a0 <= b1, a1 >= b1]
	g (a0,a1) (b0,b1) = a0 > b1
	contains (a0,a1) (b0,b1) = and [a0 <= b0, a1 >= b1]
	isContained a b = contains b a

-- Example 
-- a = ('a','c') :: Interval Char
-- b = ('b','d') :: Interval Char
-- l a b
-- > False
-- leq a b
-- > True

compareInterval :: Ord a => Interval a -> Interval a -> Compare
compareInterval a b
	| l a b = L
	| leq a b = LEQ
	| geq a b = GEQ
	| g a b = G
	| contains a b = Contains
	| isContained a b = IsContained


-- for testing -- 
pi2 :: (a,a,a) -> a
pi2 (x,y,z) = y

-- Interval Int
a = (16,25)
b = [(1,4), (8,12), (15, 20), (24, 30), (33,38), (41, 47)]
c = triage a b
d = pi2 c

-- Interval Char
a' = ('k','m')
b' = [('a','d'), ('h','l'),('m', 'p'), ('r','z')]
c' = triage a' b'
d' = pi2 c'
-- || -- 

-- NOTE: we are going to foldr so triage will progress from the back of the list of sessions to the front
-- this means that if we cons on each new pair (y,c) to the appropriate subblist, the smaller intervals will go at the front
-- this is exactly what we want! 
-- Adding the new element as a singleton list to the end of the sublist, subList ++ [(y,c)], in addition to reversing order,
-- is also inefficient because it requires stepping all the way through subList.
triageFold :: Ord a => Interval a -> Interval a -> ([(Interval a, Compare)], [(Interval a, Compare)],[(Interval a, Compare)]) -> ([(Interval a, Compare)], [(Interval a, Compare)],[(Interval a, Compare)])
triageFold x y (gs, as, ls)
	| c == G = ((y,c):gs, as, ls)
	| c == L = (gs, as, (y,c):ls)
	| otherwise = (gs, (y,c):as, ls)
	where c = compareInterval x y 

triage :: Ord a => Interval a-> [Interval a] -> ([(Interval a, Compare)], [(Interval a, Compare)],[(Interval a, Compare)])
triage x ys = foldr (triageFold x) ([],[],[]) ys

-- optimization: can we consolidate some of these patterns?
-- Also, get the last element without reversing the list. That's costly.
-- Do we want to use an array instead of a list? 
-- Maybe it doesn't matter, but computing the length of a list also requires stepping through it.
absorb :: Ord a => Interval a -> [(Interval a, Compare)] -> [Interval a]
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
-- This is the tuple version of zipWith ($) [f,g,h] [a,b,c].
-- Actually, we should optmize this because the two map calls step through each of those subLists
-- then adding the lists also requires stepping through the subLists.
-- this may or may not be worth it.
update :: Ord a => Interval a -> [Interval a] -> [Interval a]
update x ys = (map fst a) ++ (absorb x b) ++ (map fst c)
	where (a, b, c) = triage x ys


