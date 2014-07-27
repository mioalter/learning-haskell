
import Data.Maybe

--Exercise 1: Hopscotch
--Your first task is to write a function
--skips :: [a] -> [[a]]
--The output of skips is a list of lists. 
--The first list in the output should be the same as the input list. 
--The second list in the output should contain every second element from the input list. . . 
--and the nth list in the output should contain every nth element from the input list.

unsafeTakeNth :: Int -> [a] -> a
unsafeTakeNth n = head . drop (n-1) . take n

takeNth :: Int -> [a] -> Maybe a
takeNth n xs
	| length xs < n || length xs < 1 = Nothing
	| otherwise = Just (unsafeTakeNth n xs)

takeEveryNthMaybe :: Int -> [a] -> [Maybe a]
takeEveryNthMaybe n xs
	| length xs < n || length xs < 1 = [Nothing] 
	| otherwise = (takeNth n xs) : takeEveryNthMaybe n (drop n xs)

takeEveryNth :: Int -> [a] -> [a]
takeEveryNth n xs = map fromJust (filter isJust (takeEveryNthMaybe n xs))

skips :: [a] -> [[a]]
skips xs = [takeEveryNth n xs | n<-[1..length xs]]