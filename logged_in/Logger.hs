{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Updater
import AParser
import Control.Monad
import Data.List.Split
import Control.Applicative

type Userid = Int

data User = User {userid :: Userid, sessions :: [Interval Int], errors :: [Interval Int]}
	deriving (Eq, Show)

intCombiner :: Int -> Int -> Int -> (Userid, Interval Int)
intCombiner a b c = (a, (b, c))

rowParser :: Parser (Userid, Interval Int)
rowParser = (pure intCombiner) <*> posInt' <*> posInt' <*> posInt'

updateUser :: User -> (Userid, Interval Int) -> User
updateUser u (a, (b,c))
	| b <= c = User {userid = userid u, sessions = update (b,c) (sessions u), errors = errors u}
	| otherwise = User {userid = userid u, sessions = sessions u, errors = (b,c) : (errors u)}

rob :: User 
rob = User {userid = 123, sessions = [(1,3), (10,14), (20,25), (32,45)], errors = [(5,3), (100,10)]}




-- Question: what are the differences between type, newtype, and data again?

{-
Question: we want to update sessions when we get a legit new session and log errors if we get a bogus session ((a,b) where a > b).
It seems better to create one User data structure that keeps track of both rather than having good updates and bad updates go
to different places. Then we can just write out everything at once.
Does that make sense? Or are we making our User data structure too complicated? This is easy, we'll just add another list
of intervals to hold the bad sessions.

-}

-- want to load tsv with cols userid, start_time, end_time and make a tuple (userid, (start_time, end_time))
-- can we load the whole file as a string and then split on \n?
-- or is there a readlines?
-- We actually want something of type Filepath -> (Int, Interval Int)
-- we could make
--loader :: Filepath -> [(Userid, Interval Int)]
--loader f =

--tsvParser :: String -> Maybe (Userid, Interval Int))
--tsvParser = 

{- Have to decide: how do we treat bad lines? Do we make our parser return Maybe (Userid, Interval) and the original unparsed string
and if the first part is Nothing, then it just adds the string to a list of errors and otherwise it discards the unparsed string? -}

--f = "123\t3\t4\n123\t5\t32\n123\t38\t44\n"
--rows = lines f
--pRows = map (parseHelper . tsvParser) rows
--pedRows = map rowParser pRows

--parseHelper :: [String] -> Maybe [String]
--parseHelper [(a:as), (b:bs), (c:cs)] = Just [(a:as), (b:bs), (c:cs)]
--parseHelper _ = Nothing

--tsvParser :: String -> [String]
--tsvParser s = splitOn "\t" s 

--sessionParser :: [String] -> (Userid, Interval Int)
--sessionParser [a, b, c] = (read a :: Int, (read b :: Int, read c :: Int)) 

--rowParser :: Maybe [String] -> Maybe (Userid, Interval Int)
--rowParser xs = fmap sessionParser $ xs

