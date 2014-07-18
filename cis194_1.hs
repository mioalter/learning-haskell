
---Exercise 1
--v1
--headDigit :: Integer -> Integer
--headDigit n = read (take 1 (show n)) :: Integer

--tailDigits :: Integer -> Integer
--tailDigits n = read (drop 1 (show n)) :: Integer
---breaks for integers with < 2 digits, but that's okay because 
---innerToDigits does the checking

---broken! have head and tail return strings and only cast as a list of integers at the end!!
---maybe make headDigit and tailDigits of type Integer --> [[Char]]

--innerToDigits :: Integer -> [Integer]
--innerToDigits n = if n `elem` [0..9] then [n] else (headDigit n) : innerToDigits (tailDigits n)

--toDigits :: Integer -> [Integer]
--toDigits n = if n < 1 then [] else innerToDigits n

--v2
innerToDigits :: [Char] -> [[Char]]
innerToDigits n = if length(n)==1 then [n] else [(head n)] : innerToDigits (tail n)
--note: 
--head "1234" is the character '1', 
--[head "1234"] is the string "1"

toDigits :: Integer -> [Integer]
toDigits n = if n < 1 then [] else [read x :: Integer | x <- innerToDigits (show n)]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

---Exercise 2
doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft [x] = [x]
doubleEveryOtherLeft (x:y:xs) = (x : (2*y) : (doubleEveryOtherLeft xs))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherLeft (reverse xs))

---Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = sum (toDigits x)
sumDigits (x:xs) = (sum (toDigits x)) + sumDigits xs

---Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

