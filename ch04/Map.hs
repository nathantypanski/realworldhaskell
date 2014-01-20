-- file: ch04/Map.hs
-- pg. 87
import Data.Char (toUpper)

square :: [Double] -> [Double]
square (x:xs) = x*x : square xs
square []     = []

upperCase :: String -> String
upperCase (x:xs) = toUpper x : upperCase xs
uppercase []     = []

square2 :: [Double] -> [Double]
square2 xs = map squareOne xs
  where squareOne x = x * x

upperCase2 :: String -> String
upperCase2 xs = map toUpper xs

myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _      = []
