
-- Credit card number validation
-- We want to: 
-- (1) convert an integer (credit card number) to a list of digits
-- (2) double every second digit starting from the right
-- (3) sum all resulting digits, meaning, if any digits double to two-digit numbers, sum the digits of those, too
-- (4) validate: the sum must be 0 mod 10 to be valid

---Exercise 1
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
sumDigits (x:xs) = sumDigits [x] + sumDigits xs

---Exercise 4
validate :: Integer -> Bool
validate n = sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0

--Towers of Hanoi
-- Note: if hanoi n a b c is the solution for n>3 discs starting on peg a and ending on peg c
-- then 
-- hanoi n a b c is: 
-- (i) move the (n-1) top discs from a to c, 
-- (ii) move the biggest disc from a to b, 
-- (iii) move the (n-1) discs from c to a
-- (iv) move the biggest disc from b to c
-- (v) move the (n-1) discs from a to c
--Exercise 5
type Peg = String
type Move = (Peg,Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,c)]
hanoi 2 a b c = [(a,b), (a,c), (b,c)]
hanoi 3 a b c = [(a,c), (a,b), (c,b), (a,c), (b,a), (b,c), (a,c)]
hanoi n a b c = concat [hanoi (n-1) a b c, [(a,b)], hanoi (n-1) c b a, [(b,c)], hanoi (n-1) a b c]



