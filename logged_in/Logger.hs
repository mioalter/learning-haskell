{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Updater

data User = User {userid :: Int, sessions :: [Interval Int], errors :: [Interval Int]}
	deriving (Eq, Show)

{-
Question: we want to update sessions when we get a legit new session and log errors if we get a bogus session ((a,b) where a > b).
It seems better to create one User data structure that keeps track of both rather than having good updates and bad updates go
to different places. Then we can just write out everything at once.
Does that make sense? Or are we making our User data structure too complicated? This is easy, we'll just add another list
of intervals to hold the bad sessions.

-}



