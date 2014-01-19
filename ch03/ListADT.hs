-- recursive data type
-- implementation of list.
-- Cons -> :
-- Nil  -> []

data List a = Cons a (List a)       -- We have `Cons a` and a repeat of `List`.
            | Nil                   -- Note: Nil has a list type.
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

-- Ch 03 "Recursive Types" exercise 1

toList :: List a -> [a]
toList (Cons x xs) = (x:toList xs)
toList Nil = []
