{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Monoid
import Sized
import Scrabble
import Buffer
-- Unrelated exercise: writer splitter function for balancing a dataset in haskell.
-- see splitter.hs

data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- | Exercise 1

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

-- | Exercise 2

-- use this helper function in indexJ, dropJ and takeJ!
annToInt :: (Sized b, Monoid b) => JoinList b a -> Int
annToInt = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n Empty = Nothing
indexJ n (Single x y) 
				| n == 0 = Just y
				| otherwise = Nothing
indexJ n (Append x l r)
				| n > (getSize . size $ x) = Nothing
				| n > (getSize . size . tag $ l) = indexJ (n - num_left) r
				| otherwise = indexJ n l
					where num_left = getSize . size . tag $ l 

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n t 
		| n > (getSize . size . tag $ t) = Empty
		| n < 1 = t
dropJ n (Append _ l r)
		| n > (getSize . size . tag $ l) = dropJ (n - num_left) r
		| otherwise = (+++) (dropJ n l) r
			where num_left = getSize . size . tag $ l 

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n t
		| n > (getSize . size . tag $ t) = t
		| n < 1 = Empty
takeJ n (Append _ l r)
		| n < (getSize . size . tag $ l) = takeJ n l
		| otherwise = (+++) l (takeJ (n - num_left) r)
			where num_left = getSize . size . tag $ l

-- Testing

(!!?) :: [a] -> Int -> Maybe a
(!!?) [] _ = Nothing
(!!?) _ i | i < 0 = Nothing
(!!?) (x:xs) i 
			| i == 0 = Just x
			| otherwise = (!!?) xs (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- | Exercise 3
scoreLine :: String -> JoinList Score String
scoreLine x = Single (scoreString x) x
 
-- | Exercise 4
instance Buffer (JoinList (Score, Size) String) where
	toString = unwords . jlToList
	fromString x = Single (scoreString x, Size 1) x
	line = indexJ
	replaceLine n x b = takeJ (n-1) b +++ fromString x +++ dropJ (n+1) b
	numLines = getSize . size . tag
	value = getScore . fst . tag

