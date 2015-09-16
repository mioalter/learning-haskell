{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

threeInts :: Rand StdGen (Int, Int, Int)
 threeInts =
   getRandom >>= \i1 ->
   getRandom >>= \i2 ->
   getRandom >>= \i3 ->
   return (i1,i2,i3)

 threeInts' :: Rand StdGen (Int, Int, Int)
 threeInts' = do
 	i1 <- getRandom
 	i2 <- getRandom
 	i3 <- getRandom
 	return (i1,i2,i3)  



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
