\begin{code}
import Control.Monad
import Data.Char
--main = do   
--    line <- getLine  
--    if null line  
--        then return ()  
--        else do  
--            putStrLn $ reverseWords line  
--            main  
  
--reverseWords :: String -> String  
--reverseWords = unwords . map reverse . words  

--main = do     
--    c <- getChar  
--    if c /= ' '  
--        then do  
--            putChar c  
--            main  
--        else return ()
\end{code}

same as above, but uses 'when' rather than 'if...else'  
\begin{code}
--main = do  
--    c <- getChar  
--    when (c /= ' ') $ do  
--        putChar c  
--        main 


--main = forever $ do  
--    putStr "Give me some input: "  
--    l <- getLine  
--    putStrLn $ map toUpper l  

--main = do   
--    colors <- forM [1,2,3,4] (\a -> do  
--        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
--        getLine)  
--    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
--    mapM_ putStrLn colors 

\end{code}

 I would think that
return :: a -> IO a
but according to ghci
return :: Monad m => a -> m a
LYAH says that return makes an IO action out of a pure value
ah, okay, that makes sense. IO is a moand and here
return :: a -> IO a, like I said. Okay.

so return is the opposite of <-
 <- converts an IO a into an a and 
 return converts an a into an IO a 
 if ghci let me type ":t <-" i'll bet it would say
 <- :: IO a -> a 

***********************
** Files and Streams **
***********************

getContents :: IO String
reads all of the contents of a file, but lazily! It doesn't read everything into memory,
it only reads it in when you need it.

\begin{code}
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
\end{code}

We can condense

\begin{code}

main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)

\end{code}

into just

\begin{code}

main = interact shortLinesOnly

\end{code}

