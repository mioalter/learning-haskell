import System.IO  
  
main = do  
    handle <- openFile "gf.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle 