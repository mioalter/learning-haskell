--main = putStrLn "Hello, Haskell!"
--main = putStrLn "Hello" >> putStrLn "world!"

main :: IO ()
main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))