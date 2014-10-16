import Control.Applicative

data Maybe' a = Nothing | Maybe' a

instance Applicative Maybe' where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something 