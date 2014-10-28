{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid
import Data.Char

newtype Score = Score Int
	deriving (Eq, Ord, Show, Num)

-- get letter scores from http://www.thepixiepit.co.uk/scrabble/rules.html

instance Monoid Score where
	mempty = Score 0
	mappend = (+)

getScore :: Score -> Int
getScore (Score n) = n

score :: Char -> Score
score x
	| y `elem` ['A', 'E', 'I', 'O',  'U',  'L',  'N',  'R',  'S',  'T'] = 1
	| y `elem` ['D',  'G']  = 2
	| y `elem` ['B',  'C',  'M',  'P'] = 3
	| y `elem` ['F',  'H',  'V',  'W',  'Y'] = 4
	| y `elem` ['K'] = 5
	| y `elem` ['J',  'X'] = 8
	| y `elem` ['Q',  'Z'] = 10
	| otherwise = 0
		where y = toUpper(x)

scoreFoldr :: Char -> Score -> Score
scoreFoldr x n = score x <> n

scoreString :: String -> Score
scoreString = foldr (scoreFoldr) (Score 0)