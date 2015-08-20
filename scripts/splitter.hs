{-

Given a TSV of data with one column (the last) whose values are 'train' and 'test'
write a script to split the file in two.


to list installed packages
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
	| lst == Just "test" = (tr, s:ts)
	| otherwise	= (tr, ts)
	where lst = getLast $ wordsBy (=='\t') s

splitsky :: [String] -> ([String], [String])
splitsky = foldl splitHelper ([],[])

{-
Note: using foldr preserves the order of the lines in the input file
whereas using foldl reverses them BUT foldr will build up a thunk
the size of the input file while foldl will process as it goes
(foldl is tail recursive) so foldl is actually what you want to use

(note that the arguments to the folding functions for foldr and foldl
are reversed: for foldl, the accumulator comes first)

splitHelper :: String -> ([String],[String]) -> ([String],[String])
splitHelper s (tr, ts)
	| lst == Just "train" = (s:tr, ts)
	| otherwise	= (tr, s:ts)
	where lst = getLast $ wordsBy (=='\t') s

splitsky :: [String] -> ([String], [String])
splitsky = foldr splitHelper ([],[])
-}

pipeLine :: [String] -> (String, String)
pipeLine xs = (unlines $ header : trainLines, unlines $ header : testLines)
	where 
		header = head xs
		(trainLines, testLines) = splitsky xs

main :: IO()
main = do 
	opts <- cmdArgs options
	allLines <- fmap lines $ readFile $ inFile opts
	let (trainUnlines, testUnlines) = pipeLine allLines
	let (trainName, testName) = (outName f "train", outName f "test") where f = inFile opts
	writeFile trainName trainUnlines
	writeFile testName testUnlines

