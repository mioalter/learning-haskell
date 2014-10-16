
--see http://apfelmus.nfshost.com/articles/monoid-fingertree.html

-- We will create a tree-based data structure to store the elements of a list
-- so that we can look up the nth element in O(log n) time rather than O(n)

-- v is the annotation at each branch, 
-- (this will be the number of leaves below that branch in the tree)
-- a is the list element
data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)
              deriving Show

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

-- look up the annotation at a given branch
tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v

-- look up the first element of the tree/list
head' :: Tree v a -> a
head' (Leaf _ a)     = a
head' (Branch _ x _) = head' x

-- a type synonym, we'll use this for the annotations v
type Size = Int

-- we want the annotations to indicate the number of leaves below the current branch
-- this means we want to impose the relations
-- tag (Leaf _ _) = 1 and
-- tag (Branch _ x y) = tag x + tag y
-- we impose these by creating new leaf and branch constructors
-- that set the annotations according to these relations

-- our new constructors will be for trees of type Tree Size a
-- and we'll set the Size according to the relations
leaf :: a -> Tree Size a
leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch (tag x + tag y) x y

-- our tree look-up function!
-- retrieve the nth element
(!!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !!! 0 = a
(Branch _ x y)  !!! n 
     | n < tag x     = x !!! n
     | otherwise     = y !!! (n - tag x)

-- this is a helper funtion for toTree
-- it tells us on what index to split the list in two
split :: [a] -> Int
split = (`div` 2) . length

-- now we can make a tree out of any list of things of typeclass Show
-- we don't need to handle the empty list, we will only call
-- toTree recursively on lists of length > 2, by the time we get down 
-- to lists of length 1 or 2, we'll hit the base cases.
toTree :: Show a => [a] -> Tree Size a
toTree [x] = leaf x
toTree [x, y] = branch (leaf x) (leaf y)
toTree xs = branch (toTree $ take (split xs) xs) (toTree $ drop (split xs) xs)


