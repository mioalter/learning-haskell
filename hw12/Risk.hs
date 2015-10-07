{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

--threeInts :: Rand StdGen (Int, Int, Int)
--threeInts =
--   getRandom >>= \i1 ->
--   getRandom >>= \i2 ->
--   getRandom >>= \i3 ->
--   return (i1,i2,i3)

--threeInts' :: Rand StdGen (Int, Int, Int)
--threeInts' = do
--	i1 <- getRandom
--	i2 <- getRandom
--	i3 <- getRandom
--	return (i1,i2,i3)  


------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
	deriving (Eq,Show)


numAttackers :: Battlefield -> Int
numAttackers (Battlefield a _)
	| a > 3 = 3
	| a == 3 = 2
	| a == 2 = 1
	| otherwise = 0

numDefenders :: Battlefield -> Int
numDefenders (Battlefield _ d)
	| d >= 2 = 2
	| d == 1 = 1 
	| otherwise = 0


-- we do all this inside the Rand StdGen monad, then return the new Battlefield.
-- make a function of type Rand StdGen [Int]
-- inside the Rand StdGen monad, execute one for the attackers, one for the defenders, sort the lists and zip them
-- write a function that takes a BattleField and a pair of DieValues and produces a new Battlefield
-- apply that function twice with the two die rolls
-- return the new Battlefield.

strike :: Battlefield -> (DieValue, DieValue) -> Battlefield
strike bf (DV a,DV b)
	| a > b = Battlefield {attackers = attackers bf, defenders = (defenders bf) - 1}
	| otherwise = Battlefield {attackers = (attackers bf) - 1, defenders = defenders bf}


dieRolls :: Int -> Rand StdGen [DieValue]
dieRolls n = do
	a <- die
	b <- die
	c <- die
	return $ reverse . sort $ take n [a,b,c]

-- Problem! Rolling two dice (for the defenders) is not the same as folling three dice, sorting, and taking the first two
-- which is what we are doing now by rolling three times, sorting, zipping, adn then taking numRolls.
-- So, again, we have to actually know the number of attackers or defenders
-- so just add a Int parameter
-- and do a (take n) *before* sorting.
-- okay, fised.

rollPairs :: Int -> Int -> Rand StdGen [(DieValue,DieValue)]
rollPairs a d = do
	attackRolls <- dieRolls a
	defendRolls <- dieRolls d
	return $ zip attackRolls defendRolls

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
	let a = numAttackers bf
	let d = numDefenders bf
	rps <- rollPairs a d 
	return $ foldl strike bf rps

continueFighting :: Battlefield -> Bool
continueFighting bf 
	| ((attackers bf) > 2) && (defenders bf) > 0 = True
	| otherwise = False

--invade :: Battlefield -> Rand StdGen Battlefield
--invade bf
--	| continueFighting bf == True = do bf' <- battle bf; invade bf'
--	| otherwise = return bf

attackersWin :: Battlefield -> Double
attackersWin bf 
	| defenders bf == 0 = 1.0
	| otherwise = 0.0

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = case (continueFighting bf) of 
	True -> (battle bf) >>= invade
	False -> return bf

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
	let initialBattlefield = take 1000 $ repeat bf -- :: [Battlefield]
	let battles = map invade initialBattlefield -- :: [Random StdGen Battlefield]
	outcomes <- sequence battles -- sequence battles :: Random StdGen [Battlefield]
	let verdicts = map attackersWin outcomes
	return $ (sum verdicts) / 1000

-- returns 0! All the time! Problem! Okay, let's try running individual functions to make sure they are working properly.
b = Battlefield 10 10 
b' = Battlefield 100 10

makeBattlefield :: Int -> Battlefield
makeBattlefield n = Battlefield n 10

-- I see the problem: 
-- in rollPairs, we have dieRolls a and dieRolls d where a and d are the numbers 
-- no, actually, tha's right.
-- Looks like it works

-- yes>>>sequence [evalRandIO $ successProb (makeBattlefield i) | i <- [10, 20, 30, 40, 50, 60, 70, 80, 90]]
-- [9.0e-3,0.144,0.459,0.757,0.899,0.962,0.994,1.0,1.0]

-- the problem is just that with equal numbers of atackers and defenders, since the dice rolling rules favor the defenders, the attackers usually lose
-- ....unless I got the rules wrong? No, I think they're right.

-- Huh, originally had sort rather than reverse.sort on the die rolls.
-- sorting properly means attackers are much more likely to win.
-- Okay, this makes sense: attackers are slightly less than 50% likely to win when starting with equal numbers
-- because ties go to defenders
-- but the discrepancy isn't huge and probability quickly goes to 1 as number of attackers increases.
-- Triumphs of modular design: very easy to update.
