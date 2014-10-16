-- see http://apfelmus.nfshost.com/articles/monoid-fingertree.html

-- We'll make a priority queue to store things of different urgencies
-- we'll denote urgency by time until something must happen
-- so smaller times are more urgent

-- now, instead of annotating each branch with the number of leaves below it
-- we'll annotate each branch with the most urgent (smallest urgency) of all
-- leaves below it
-- thus, at each branch, we'll know which subtree contains the most
-- urgent task

-- note that now, the objects we are storing must have a value (a task) and
-- and a priority (urgency) so we'll assume that they come with their own functions
-- priority :: a -> Int 
-- task :: a -> String, say.
data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)
              deriving Show

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v

-- again, a type synonym that we'll use for the annotations in our tree
type Priority = Int

-- now, the relations on annotations are
-- tag (Leaf _ a) = priority a 
-- tag (Branch _ x y) = tag x `min` tag y 
-- because the priority at a branch is the minimum priority of either subtree

leaf :: a -> Tree Priority a
leaf a = Leaf (priority a) a

branch :: Tree Priority a -> Tree Priority a -> Tree Priority a
branch x y = Branch (tag x `min` tag y) x y

winner :: Tree Priority a -> a
winner t = go t
    where
    go (Leaf _ a)        = a
    go (Branch _ x y)
        | tag x == tag t = go x   -- winner on left
        | tag y == tag t = go y   -- winner on right

