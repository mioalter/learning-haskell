
import Data.Maybe

--Exercise 1: Hopscotch
--Your first task is to write a function
--skips :: [a] -> [[a]]
--The output of skips is a list of lists. 
--The first list in the output should be the same as the input list. 
--The second list in the output should contain every second element from the input list. . . 
--and the nth list in the output should contain every nth element from the input list.

--take' :: Int -> [a] -> Maybe [a]
--take' n [] = Nothing
--take' n (x:xs) = Just (drop (n-1) xs)

unsafeTakeNth :: Int -> [a] -> a
unsafeTakeNth n = head . drop (n-1) . take n

takeNth :: Int -> [a] -> Maybe a
takeNth n xs
	| length xs < n || length xs < 1 = Nothing
	| otherwise = Just (unsafeTakeNth n xs)

takeEveryNth :: Int -> [a] -> [Maybe a]
takeEveryNth n xs
	| length xs < n || length xs < 1 = [Nothing] 
	| otherwise = (takeNth n xs) : takeEveryNth n (drop n xs)

takeEveryNthComplete :: Int -> [a] -> [a]
takeEveryNthComplete n xs = map fromJust (filter isJust (takeEveryNth n xs))

--skips :: [a] -> [[a]]
