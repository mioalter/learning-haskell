{-# LANGUAGE OverloadedStrings #-}         
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


-- Should we not have all these functions of type -> IO ()? Would it be better to write pure functions and save the IO part for inside a do block? It is, right? Because that's how we sequence things.
-- No, IO is a monad, we want to sequence functions of type a -> IO b 
-- Okay, so we'll make the pure parts of these their own functions, the better to set parameters (what columns we want)
-- then we can make the IO functions of type Turtle.FilePath -> (Shell Text -> Shell Text) -> IO ()

defaultModel = "GBMModel__9527db7b50c56f078d0528b8b09840a6" :: Model
defaultFile = "testSH.tsv" :: Filename
defaultFilePath = "testSH.tsv" :: Turtle.FilePath

toCSV :: Turtle.FilePath -> IO ()
toCSV f =  output (f <.> "csv") $ inshell "cut -d, -f2-70" $ sed ("\t" *> return ",") $ input f

toIdsLabels :: Turtle.FilePath -> IO ()
toIdsLabels f = output (f <.> "ids_labels") $ inshell "cut -d, -f1,73,77" $ sed ("\t" *> return ",") $ input f

toScores :: Turtle.FilePath -> IO ()
toScores f = output (f <.> "scores") $ inshell "cut -d, -f3" $ input f

--zipFiles :: Turtle.FilePath -> Turtle.FilePath

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
	toCSV defaultFilePath
	toIdsLabels defaultFilePath
	shell (preScoreCommand defaultModel) empty
	shell (scoreCommand defaultModel (defaultFile <> ".csv")) empty


