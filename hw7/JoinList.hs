{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

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
jlSizedToInt :: (Sized b, Monoid b) => JoinList b a -> Int
jlSizedToInt = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n t | n < 0 || n > (jlSizedToInt t) = Nothing
indexJ _ Empty = Nothing
indexJ n (Single x y)
				| n == 0 = Just y
				| otherwise = Nothing
indexJ n (Append x l r)
				| n >= num_left = indexJ (n - num_left) r
				| otherwise = indexJ n l
					where num_left = jlSizedToInt l 

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n t 
		| n > (jlSizedToInt t) = Empty
		| n < 1 = t
dropJ n (Single x y) = Empty
dropJ n (Append _ l r)
		| n > num_left = dropJ (n - num_left) r
		| otherwise = (+++) (dropJ n l) r
			where num_left = jlSizedToInt l 

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n t
		| n > (jlSizedToInt t) = t
		| n < 1 = Empty
takeJ n (Single x y)
		| n == 1 = Single x y
		| otherwise = Empty
takeJ n (Append _ l r)
		| n < num_left = takeJ n l
		| otherwise = (+++) l (takeJ (n - num_left) r)
			where num_left = jlSizedToInt l

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
	replaceLine n x b = takeJ n b +++ fromString x +++ dropJ (n + 1) b
	numLines = jlSizedToInt
	value = getScore . fst . tag

-- Looks like there's an error in the indexJ function so the 'line' function
-- is wonky, the other functions in the above instance work fine.
initialBuffer :: JoinList (Score, Size) String
initialBuffer = foldr (+++) Empty (map fromString 
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ])

main = runEditor editor $ initialBuffer

