{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

mergeSorted :: ([Int],[Int]) -> [Int]
mergeSorted ([],[]) = []
mergeSorted (xs,[]) = xs
mergeSorted ([],ys) = ys
mergeSorted ((x:xs),(y:ys)) 
	| x <= y = x : mergeSorted (xs, (y:ys))
	| otherwise = y : mergeSorted ((x:xs), ys)

-- this is super inefficient because 
-- you have to step through the whole list to divide it in half
-- we should definitely use an *array*, not a list
split :: [Int] -> ([Int],[Int])
split x = (take n x, drop n x)
	where n = fromIntegral $ length(x) `div` 2

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeSorted (mergeSort a, mergeSort b)
	where (a,b) = split xs