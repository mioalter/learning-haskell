
data Tree v a = Leaf   v a
              | Branch v (Tree v a) (Tree v a)