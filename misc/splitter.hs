
-- write as a fold
-- will need accumulator to keep track of index in the list.

-- the accumulator will be the greatest list index to date and the pair of lists
-- so will be of type (Integer, [Integer], [Integer])
helper :: (Integer, [Integer], [Integer]) -> Integer -> (Integer, [Integer], [Integer])
helper (n, xs, ys) e
				| e == 1 = (n+1, (n+1) : xs, ys)
				| otherwise = (n+1, xs, (n+1) : ys)

splitter :: [Integer] -> (Integer, [Integer], [Integer])
splitter = foldl helper (-1, [], [])