{-# LANGUAGE OverloadedStrings #-}         
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import Turtle

type Model = Text
type Filename = Text

-- A receptacle for command line arguments (CLAs)
data Options = Options {
	inModel :: String
	, inFile :: String
	, scoreCols :: String
	, valCols :: String
} deriving (Data, Typeable)

-- a set of default values for CLAs
options :: Options
options = Options {
	inModel = "GBMModel__9527db7b50c56f078d0528b8b09840a6"
		&= typ "MODEL FOR SCORING"
		&= help "Model name (without extension)"
	, inFile = "testSH.tsv"
		&= typ "INPUT FILENAME"
		&= help "A TSV to score"
	, scoreCols = "2-70"
		&= typ "COLUMNS TO SCORE"
		&= help "input cols to model e.g. 2-70"
	, valCols = "1,73,77"
		&= typ "VALIDATION COLUMNS"
		&= help "such as id, label, matched, date e.g. 1,73,77"
	}
	&= summary "A program to score a TSV with a trained H2O model"
	&= program "POJO Pal v1 (c) Mio"

--toCSV :: Text -> Shell Text -> Shell Text
---- convert TSV to CSV and select a subset of columns
--toCSV t = inshell ("cut -d, -f" <> t) . sed ("\t" *> return ",")

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

toCSV2 :: Text -> Filename -> Filename -> Text
toCSV2 cols infile outfile = mconcat l 
	where l = [
		"cat " <> infile
		, " | "
		, "tr $'\t' ,"
		, " | "
		, " cut -d, -f"
		, cols
		, " > " <> outfile
		]

zipHelper :: String -> String -> String
zipHelper a b = a ++ "," ++ b

csvZipper :: [String] -> [String] -> [String]
csvZipper as bs = zipWith zipHelper as bs

main = do
	opts <- cmdArgs options
	let f = fromString $ inFile opts :: Filename
	let model = fromString $ inModel opts :: Model
	let sCols = fromString $ scoreCols opts :: Text
	let vCols = fromString $ valCols opts :: Text
	let fCSV = f <> ".csv"
	let fIDs = f <> ".ids_labels"
	let fScoreD = f <> ".csv_scored"
	let fScores = fScoreD <> ".scores"
	shell (toCSV2 sCols f fCSV) empty
	shell (toCSV2 vCols f fIDs) empty
	shell (preScoreCommand model) empty
	shell (scoreCommand model fCSV) empty
	shell (toCSV2 "3" fScoreD fScores) empty
	let gIDs = inFile opts ++ ".ids_labels" :: Prelude.FilePath
	let gScores = inFile opts ++ ".csv_scored" ++ ".scores" :: Prelude.FilePath
	let gCombined = inFile opts ++ ".scores_and_labels.csv" :: Prelude.FilePath
	idLines <- fmap lines $ readFile gIDs
	scoreLines <- fmap lines $ readFile gScores
	let combinedLines = csvZipper idLines scoreLines
	writeFile gCombined $ unlines combinedLines



