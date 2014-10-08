
--see http://apfelmus.nfshost.com/articles/monoid-fingertree.html

data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)

toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

tag :: Tree v a -> v
tag (Leaf v _)     = v
tag (Branch v _ _) = v

head' :: Tree v a -> a
head' (Leaf _ a)     = a
head' (Branch _ x _) = head' x

type Size = Int

leaf :: a -> Tree Size a
leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch (tag x + tag y) x y

(!!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !!! 0 = a
(Branch _ x y)  !!! n 
     | n < tag x     = x !!! n
     | otherwise     = y !!! (n - tag x)

split :: [a] -> Int
split = (`div` 2) . length

toTree :: [a] -> Tree Size a
toTree [x] = leaf x
toTree [x, y] = branch (leaf x) (leaf y)
toTree [x, y, z] = branch (toTree [x,y]) (leaf z) --unnecessary once we define recursively
--toTree (x:xs)

