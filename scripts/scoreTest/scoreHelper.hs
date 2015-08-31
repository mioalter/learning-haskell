{-# LANGUAGE OverloadedStrings #-}         
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Turtle

{-

List of steps
* for each file: select columns and make a csv
* for each file: score
* for each file: select id column from originalFile, select scores column from scoreFile
* load into Python, zip together -> instead, zip together the files in this script and adapt the stats script

-- BETTER: make one file with all dates in it. 
-}

-- I. --
-- Write a function of type Turtle.FilePath -> IO () which does the equivalent of cat <filename> | tr $'\t' ',' | cut -d, -f<columns> > <filename>.csv

type Model = Text
type Filename = Text

defaultModel = "GBMModel__9527db7b50c56f078d0528b8b09840a6" :: Model
defaultFile = "testSH.tsv" :: Filename
defaultFilePath = "testSH.tsv" :: Turtle.FilePath

--data Options = Options {
--	inModel :: Model
--	, inFile :: Filename
--	, inFilePath :: Turtle.FilePath
--} deriving (Data, Typeable)

--options :: Options
--options = Options {
--	inModel = "GBMModel__9527db7b50c56f078d0528b8b09840a6"
--		&= typ "MODEL FOR SCORING"
--		&= help "Model name (without extension)"
--	, inFile = "testSH.tsv"
--		&= typ "INPUT FILE NAME"
--		&= help "A TSV to score"
--	, inFilePath = "testSH.tsv"
--		&= typ "INPUT FILEPATH"
--		&= help "Same"
--	}
--	&= summary "A program to score a TSV with an trained H2O model"
--	&= program ""

toCSV :: Shell Text -> Shell Text
toCSV = inshell "cut -d, -f2-70" . sed ("\t" *> return ",")

toIdsLabels :: Shell Text -> Shell Text
toIdsLabels = inshell "cut -d, -f1,73,77" . sed ("\t" *> return ",")

toScores :: Shell Text -> Shell Text
toScores = inshell "cut -d, -f3" 

preScoreCommand :: Model -> Text
preScoreCommand model = mconcat l 
	where l = [
		"javac -cp h2o-genmodel.jar -J-Xmx2g -J-XX:MaxPermSize=256m PredictCSV.java " 
		, model <> ".java"
		]

scoreCommand :: Model -> Filename -> Text
scoreCommand model input = mconcat l
	where l = [
		"java -ea -cp .:./h2o-genmodel.jar -Xmx4g -XX:MaxPermSize=256m -XX:ReservedCodeCacheSize=256m PredictCSV --header"
		, " --model " <> model
		, " --input " <> input
		, " --output " <> input <> "_scored"
		]

main = do
	--opts <- cmdArgs options
	--let fName = inFile opts
	--let f = inFilePath opts
	--let model = inModel opts
	let f = defaultFilePath
	let fName = defaultFile
	let model = defaultModel
	let fCSV = f <.> "csv"
	let fIDs = f <.> "ids_labels"
	let fScoreD = f <.> "csv_scored"
	let fScores = fScoreD <.> "scores"
	output fCSV $ toCSV $ input f
	output fIDs $ toIdsLabels $ input f
	shell (preScoreCommand model) empty
	shell (scoreCommand model (fName <> ".csv")) empty
	output fScores $ toScores $ input fScoreD


-- Since lines :: String -> [String], I need something equivalent that works on Turtle.Text.
-- I'm sure there is a function someplace....
--zipFiles :: Turtle.FilePath -> Turtle.FilePath -> IO ()
--zipFiles f g = do
--	fLines <- lines <$> readFile f
--	gLines <- lines <$> readFile g
--	writeFile outf $ unlines $ zip fLines gLines
--		where outf = (fst $ splitExtension f) <.> "scores_and_labels"