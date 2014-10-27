
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Data.Monoid

--see http://apfelmus.nfshost.com/articles/monoid-fingertree.html

-- We will create a tree-based data structure to store the elements of a list
-- so that we can look up the nth element in O(log n) time rather than O(n)

-- v is the annotation at each branch, 
-- (this will be the number of leaves below that branch in the tree)
-- a is the list element
data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)
              deriving Show

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

-- look up the annotation at a given branch
tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v


-- a type synonym, we'll use this for the annotations v
type Size = Int


-- this is a helper funtion for toTree
-- it tells us on what index to split the list in two
split :: [a] -> Int
split = (`div` 2) . length

-- now we can make a tree out of any list of things of typeclass Show
-- we don't need to handle the empty list, we will only call
-- toTree recursively on lists of length > 2, by the time we get down 
-- to lists of length 1 or 2, we'll hit the base cases.
toTree :: Show a => [a] -> Tree Size a
toTree [x] = leaf x
toTree [x, y] = branch (leaf x) (leaf y)
toTree xs = branch (toTree $ take (split xs) xs) (toTree $ drop (split xs) xs)

instance Monoid Size where
	mempty = 0
	mappend = (+)

branch :: Monoid v => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (x <> y) x y

class Monoid v => Measured a v where
	measure :: a -> v

leaf :: Measured a v => a -> Tree v a
leaf a = Leaf (measure a) a

instance Measured a Size where
	measure _ = 1

--instance Measured a Priority where
--	measure a = priority a 

instance Measured a v => Measured (Tree v a) v where
	measure = tag

search :: Measured a v => (v -> Bool) -> Tree v a -> Maybe a
search p t
    | p (measure t) = Just (go mempty p t)
    | otherwise     = Nothing
    where
    go i p (Leaf _ a) = a
    go i p (Branch _ l r)
        | p (i <> measure l) = go i p l
        | otherwise          = go (i <> measure l) p r

(!!!) :: Tree Size a -> Integer -> a
(!!!) t k = search (> k) t





