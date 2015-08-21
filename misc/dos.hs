
f :: Int -> Maybe Bool
f x 
	| x > 0 = Just True
	| otherwise = Nothing

g :: Bool -> Maybe String
g True = Just "hashtag winning"
g False = Nothing


v = do
	x <- Just 3
	y <- f x
	g y

v' = Just 3 >>= f >>= g

w = do 
	x <- Just (-3)
	y <- f x
	g y

w' = Just (-3) >>= f >>= g

z = do
	x <- Just 3
	y <- f x
	return (x,y)
-- z = Just (3,True)

-- Exercise: rewrite z with >>=
--z' = Just 3 >>= 
{-
See notes, figured out generally what is going on
We can make out of f a function

Just 3 -> (Just 3, Just True)

then we need something that maps (Just 3, Just True) |-> Just (3,True)
that is, we want something of type

(Maybe a -> Maybe b) -> Maybe (a,b)

Conveniently, (,) :: a -> b -> (a,b)
So fmap (,) :: Maybe a -> Maybe (b -> (a,b))
This is almost what we want, but we have to use that Maybe is an applicative to map
Maybe (b -> (a,b)) -> Maybe b -> Maybe (a,b)

So fmap(,) (Just 3) :: Maybe (b -> (Int,b))
app-ing should give us something of type Maybe b -> Maybe (Int,b)
to which we can then feed Just True.

-}
