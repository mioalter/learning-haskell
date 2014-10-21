
-- Unrelated exercise: writer splitter function for balancing a dataset in haskell.
-- see splitter.hs

 data JoinList m a = Empty
                   | Single m a
                   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)