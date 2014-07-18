doubleMe x = x+x
doubleUs x y = 2*x + 2*y

doubleU x y = doubleMe x + doubleMe y

doubleSmall x = if x>100 then x else x*2

boomBang xs = [if x<10 then "Boom!" else "Bang!" | x<-xs, odd x]

length' xs = sum [ 1 | _ <- xs ]

map' f xs = [f x | x <- xs]

filter' f xs = [x | x <- xs,f x ]

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n+ sumtorial (n-1)

hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

intListLength :: [Integer] -> Integer
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1