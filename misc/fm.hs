data A = A String
data B = B String A
 
data ActionC t next = InsertC t (t -> next)
instance Functor (ActionC t) where
  fmap f (InsertC a b)= InsertC a (f . b)
 
insertC a = liftF (InsertC a id)
 
proC txt1 txt2 = do
  a <- insertC $ A txt1
  b <- insertC $ B txt2 a
  return b