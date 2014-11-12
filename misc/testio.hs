--yea>:t putStrLn
--putStrLn :: String -> IO ()
--yea>:t getLine
--getLine :: IO String

-- | only need 'do' if you have more than one I/O action.
-- | use 'let' for definitions inside a 'do' block

name2reply :: String -> String 
name2reply name = "Pleased to meet you, " ++ name ++ ".\n" ++
	"Your name contains " ++ charcount ++ " characters." 
	where charcount = show (length name)

main :: IO () 
main = do
	putStrLn "Greetings once again. What is your name?" 
	inpStr <- getLine
	let outStr = name2reply inpStr 
	putStrLn outStr

--yea>:t openFile
--openFile :: FilePath -> IOMode -> IO Handle
-- FilePath is a synonym for String

--import System.IO
--import Data.Char(toUpper)

--main = do
--	inh <- openFile "input.txt" ReadMode
--	outh <- openFile "output.txt" WriteMode
--	mainloop inh outh
--	hClose inh
--	hClose outh

--mainloop :: Handle -> Handle -> IO ()
--mainloop inh outh = 
--	do ineof <- hIsEOF inh
--		if ineof
--			then return ()
--			else do inptStr <- hGetLine inh
--				hPutStrLn outh (map toUpper inpStr)
--				mainloop inh outh