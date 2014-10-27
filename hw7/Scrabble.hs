{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid

newtype Score = Score Int
	deriving (Eq, Ord, Show, Num)

score :: Char -> Score

scoreString :: String -> Score

instance Monoid Score where
	mempty = Score 0
	mappend = (+)
