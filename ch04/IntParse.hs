-- file: ch04/IntParse.hs
-- pg. 85

import Data.Char (digitToInt) -- we'll need digitToInt shortly

asInt :: String -> Int
asInt xs = loop 0 xs

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
