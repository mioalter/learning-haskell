{-# OPTIONS_GHC -fno-warn-orphans #-}
import Data.Monoid
import Employee

--module Party where

-- | Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons e gl = GL (e:emps gl) (empFun e + totFun gl)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend a b = GL (emps a ++ emps b) (totFun a + totFun b)
  mconcat gls = GL { emps = concat $ map emps gls, totFun = foldr (+) 0 $ map totFun gls}

moreFun :: GuestList -> GuestList -> GuestList
moreFun gla glb
			| gla > glb = gla
			| otherwise = glb

-- | Exercise 2
