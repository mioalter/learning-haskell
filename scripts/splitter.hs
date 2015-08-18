{-

Given a TSV of data with one column (the last) whose values are 'train' and 'test'
write a script to split the file in two.


to list installed pakcages
mioss-mbp-3:scripts mio6si$ ghc-pkg list
so we have the cmdargs package

Following 
http://spin.atomicobject.com/2012/12/13/using-haskells-cmdargs-package/

-}

{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.FilePath.Posix
import Data.List.Split


data Options = Options {
	inFile :: FilePath
} deriving (Data, Typeable)

options :: Options
options = Options {
	inFile = "yes.tsv"
		&= typ "INPUT FILE"
		&= help "A file to split into train/test"
	}
	&= summary "A program to split a TSV with last column a train/test flag into separate training and testing sets"
	&= program "splitter"

getLast :: [a] -> Maybe a
getLast [] = Nothing
getLast (x:[]) = Just x
getLast (x:xs) = getLast xs

outName :: FilePath -> String -> FilePath
outName f s = a ++ "_" ++ s ++ b
	where (a,b) = splitExtension f

splitHelper :: ([String],[String]) -> String -> ([String],[String])
splitHelper (tr, ts) s
	| lst == Just "train" = (s:tr, ts)
	| otherwise	= (tr, s:ts)
	where lst = getLast $ wordsBy (=='\t') s

splitsky :: [String] -> ([String], [String])
splitsky = foldl splitHelper ([],[])

-- use wordsBy to split a string on a given character
-- eg. wordsBY (=='\t') "yes\tno\tmaybe"
-- > ["yes","no","maybe"]
-- there is also splitOn which takes a list of delimiters.
-- now we have to figure out how to do the IO part.

main :: IO()
main = do 
	opts <- cmdArgs options
	putStrLn $ "Gonna read " ++ (inFile opts)
	allLines <- fmap lines $ readFile $ inFile opts
	--let (trainLines, testLines) = fmap unlines $ splitsky allLines
	--let (trainName, testName) = (outName f "train", outName f "test") where f = inFile opts
	let outlines = fmap unlines $ splitsky allLines
	let outnames = (outName f "train", outName f "test") where f = inFile opts
	-- let pairs = [(fst outnames, fst outlines),(snd outnames, snd outlines)]
	putStrLn $ head $ allLines
	putStrLn $ fst outnames
	putStrLn $ snd outnames
	putStrLn $ fst outlines
	-- why is outlines :: ([String], String) ?? Should be (String, String) because we fmapped unlines.
	

