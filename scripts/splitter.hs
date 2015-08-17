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
}

outName :: FilePath -> String -> FilePath
outName f s = a ++ "_" ++ s ++ b
	where (a,b) = splitExtension f

-- use wordsBy to split a string on a given character
-- eg. wordsBY (=='\t') "yes\tno\tmaybe"
-- > ["yes","no","maybe"]
-- there is also splitOn which takes a list of delimiters.