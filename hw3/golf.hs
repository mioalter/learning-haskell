
import Data.Maybe
import Data.List

--Exercise 1: Hopscotch
--Your first task is to write a function
--skips :: [a] -> [[a]]
--The output of skips is a list of lists. 
--The first list in the output should be the same as the input list. 
--The second list in the output should contain every second element from the input list. . . 
--and the nth list in the output should contain every nth element from the input list.

unsafeTakeNth :: Int -> [a] -> a
unsafeTakeNth n = head . drop (n-1) . take n

takeNthMaybe :: Int -> [a] -> Maybe a
takeNthMaybe n xs
	| length xs < n || length xs < 1 = Nothing
	| otherwise = Just (unsafeTakeNth n xs)

takeEveryNthMaybe :: Int -> [a] -> [Maybe a]
takeEveryNthMaybe n xs
	| length xs < n || length xs < 1 = [Nothing] 
	| otherwise = (takeNthMaybe n xs) : takeEveryNthMaybe n (drop n xs)

takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n xs = map fromJust (filter isJust (takeEveryNthMaybe n xs))

skips :: [a] -> [[a]]
skips xs = [ takeEveryNth n xs | n <- [1..length xs] ]

--Exercise 2: Local Maxima
--A local maximum of a list is an element of the list which is strictly
--greater than both the elements immediately before and after it. 
--For example, in the list [2,3,4,1,5], 
--the only local maximum is 4, since it is greater than the elements 
--immediately before and after it (3 and 1). 
--5 is not a local maximum since there is no element that comes after it.
--Write a function
-- localMaxima :: [Integer] -> [Integer]
--which finds all the local maxima in the input list and returns them in order. For example:
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

unsafeTakeThree :: Int -> [a] -> [a]
unsafeTakeThree n = drop (n-3) . take n

takeThreeMaybe :: Int -> [a] -> Maybe [a]
takeThreeMaybe n xs
	| n < 3 = Nothing
	| otherwise = Just (unsafeTakeThree n xs)

takeLocalMax :: Maybe [Int] -> Maybe Int
takeLocalMax Nothing = Nothing
takeLocalMax (Just xs)
	| length xs /= 3 = Nothing
	| takeNthMaybe 3 xs >= takeNthMaybe 2 xs = Nothing
	| takeNthMaybe 1 xs >= takeNthMaybe 2 xs = Nothing
	| otherwise = takeNthMaybe 2 xs

localMaxMaybe :: [Int] -> [Maybe Int]
localMaxMaybe xs = [takeLocalMax (takeThreeMaybe i xs) | i <- [3..length xs]]

localMaxRaw :: [Int] -> [Int]
localMaxRaw xs = map fromJust (filter isJust (localMaxMaybe xs))

localMaxima :: [Int] -> [Int]
localMaxima = reverse . sort . localMaxRaw


--Exercise 3 Histogram
--For this task, write a function
-- histogram :: [Integer] -> String
--which takes as input a list of Integers between 0 and 9 (inclusive), 
--and outputs a vertical histogram showing how many of each number were in the input list. 

-- We want to count how many of each number appear and return a list whose n th number is the number of ns.

count :: Int -> [Int] -> Int
count n xs = length (filter (\x -> x == n) xs)

countAll :: [Int] -> [Int]
countAll xs = [count n xs | n <- [0..9] ]

hit :: Int -> Int -> Char
hit n m
	| n <= m = '*'
	| otherwise = ' '

hits :: Int -> [Int] -> String
hits n xs = map (hit n) xs

levels :: [Int] -> [String]
levels xs = [ hits n (countAll xs) | n <- [1..length xs] ]

format :: String -> String
format s = s ++ "\n"

formattedLevels :: [Int] -> String
formattedLevels = concat . reverse . (map format) . levels

s = "==========\n0123456789\n"

histogram :: [Int] -> IO()
histogram xs = putStr (formattedLevels xs ++ s)



