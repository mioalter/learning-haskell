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

parse :: String -> [LogMessage]
parse s = [ parseMessage x | x <- lines s ]

--Exercise 2: Organize error message in a message tree, write a function to insert new messages
getTime :: LogMessage -> Int
getTime (Unknown _) = (-1)
getTime (LogMessage (Error _) x _) = x
getTime (LogMessage _ x _) = x

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) m = m
insert l Leaf = Node Leaf l Leaf
insert l (Node Leaf root Leaf) = if getTime l >= getTime root then Node Leaf root (insert l Leaf) else Node (insert l Leaf) root Leaf
insert l (Node lTree root rTree) = if getTime l >= getTime root then Node lTree root (insert l rTree) else Node (insert l lTree) root rTree

--Exercise 3: write a function to build up a tree by successively inserting
build :: [LogMessage] -> MessageTree
build [] = Leaf
build [x] = insert x Leaf
build (x:xs) = insert x (build xs)

--Exercise 4: write a function that takes a sorted MessageTree and returns a list of log messages sorted by timestamp
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree msg rTree) = (inOrder lTree) ++ [msg] ++ (inOrder rTree)

--Exercise 5: write a function that takes an unsorted list of log messages and returns a sorted list of only those
--error messages with severity > 50.
msgIsError :: LogMessage -> Bool
msgIsError (LogMessage (Error _) _ _) = True
msgIsError (LogMessage _ _ _) = False
msgIsError (Unknown _) = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  (map show) . (filter msgIsError) . inOrder . build





