-- Reading: Real World Haskell, Chapters 2 and 3.
-- 7/19/14: resume reading with Polymorphism in lecture 3.

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

getNth :: Int -> String -> String
getNth n = concat.(drop (n-1)).(take n).words
-- note: (.) is function composition
--(words) splits a string on spaces to make a list of strings (words)
-- (take n) takes the first n elements
-- (drop (n-1)) drops the first (n-1) elements
-- so (drop (n-1)).(take n) makes a list consisting of the nth element of the list
-- (concat) concatenates nested lists of the same type into a single list of that type
-- in particular, a list of strings is a list of lists of characters
-- so we can use concat to produce a single list of characters, i.e. a string,
-- concat["yo"] == "yo"


nthToInt :: Int -> String -> Int
nthToInt n s = read (getNth n s) :: Int

restToString :: Int -> String -> String
restToString n = unwords.(drop n).words

--Exercise 1
parseMessage :: String -> LogMessage
parseMessage ('I':xs) = LogMessage Info (nthToInt 1 xs) (restToString 1 xs)
parseMessage ('W':xs) = LogMessage Warning (nthToInt 1 xs) (restToString 1 xs)
parseMessage ('E':xs) = LogMessage (Error (nthToInt 1 xs)) (nthToInt 2 xs) (restToString 2 xs)
parseMessage _ = Unknown "This is not in the right format"

