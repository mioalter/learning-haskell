{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import System.FilePath.Posix
import Data.List.Split
import Control.Parallel.Strategies

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

classify :: String -> (String, String)
classify s 
	| a == Just "train" = (s, "train")
	| a == Just "test" = (s, "test")
	| otherwise = (s, "neither")
	where a = getLast $ wordsBy (=='\t') s 

writeWhich :: (FilePath, FilePath) -> (String, String) -> IO()
writeWhich (trainOut, testOut) (s, t) 
	| t == "train" = appendFile trainOut (s++"\n")
	| t == "test" = appendFile testOut (s++"\n")
	| otherwise = putStrLn "neither"

main = do
	opts <- cmdArgs options
	allLines <- fmap lines $ readFile $ inFile opts
	let classifiedLines = map classify allLines
	let (trainName, testName) = (outName f "train", outName f "test") where f = inFile opts
	mapM_ (writeWhich (trainName, testName)) classifiedLines



