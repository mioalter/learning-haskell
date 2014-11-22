--{-# OPTIONS_GHC -fno-warn-orphans #-}
import Data.Monoid
import Employee
import Data.Tree
import Data.List

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
treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold z f t = f (rootLabel t) (map (treeFold z f) (subForest t))

bestGL :: [GuestList] -> GuestList
bestGL = foldr (\gs bs -> if totFun gs > totFun bs then gs else bs) (GL [] 0)

--tupToList :: (a,a) -> [a,a]
--tupToList (x, y) = [x, y]

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (bestGL $ map (glCons e) allgs, bestGL allgs)
	where allgs = map fst gs ++ map snd gs

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun (fst bestTwo) (snd bestTwo)
	where bestTwo = treeFold (GL [] 0, GL [] 0) nextLevel t 

-- | Exercise 5

showBest :: GuestList -> (String, [String])
showBest gl = (show $ totFun gl, map (show . empName) $ emps gl)

main = do
	companyString <- readFile "company.txt"
	let companyTree = read companyString
	let bestList = showBest $ maxFun companyTree
	putStrLn $ "Total Fun: " ++ (fst bestList)
	sequence $ map putStrLn $ sort $ snd bestList


