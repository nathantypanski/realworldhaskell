-- file: ch04/Fold.hs

{-
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl step zero (x:xs) = foldl step (step zero x) xs
foldl _    zero []     = zero
-}

myFilter p xs = foldr step [] xs
  where step x ys | p x = x : ys
                  | otherwise = ys

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr step [] xs
  where step x ys = f x : ys

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr step id xs z
    where step x g a = g (f a x)

append :: [a] -> [a] -> [a]
append xs ys = foldr (:) ys xs

foldl' _ zero [] = zero
foldl' step zero (x:xs) =
  let new = step zero x
  in new `seq` foldl' step new xs
