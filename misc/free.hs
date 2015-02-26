
import Data.Maybe

data Free f a = Pure a | Free (f (Free f a))


instance Functor f => Functor (Free f) where
	fmap g (Pure a) = Pure (g a)
	fmap g (Free as) = Free (fmap (fmap g) as)

instance Functor f => Monad (Free f) where
	return a = Pure a
	Pure a >>= g = g a
	Free fs >>= g = Free _