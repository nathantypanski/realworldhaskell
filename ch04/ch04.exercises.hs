-- file: ch04/ch04.exercises.hs
import Data.Maybe
import Data.Char (digitToInt, isNumber)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = Just $ fromJust $ safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just $ [x] ++ (fromJust $ safeInit xs)

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ []        = []
splitWith predicate (x:xs)
  | not (predicate x) = splitWith predicate xs
  | otherwise         = beginning : ending
  where
    beginning   = takeWhile predicate (x:xs)
    ending      = apply splitWith dropWhile predicate xs
    apply f h p = f p . h p

testSplitWith = splitWith notSpace phrase
                where notSpace x = x /= ' '
                      phrase = "Hello World. And all that!"

-- Ex01, pg. 97
asInt :: String -> Either String Int
asInt [] = Left "[] is not a digit"
asInt (x:xs) 
    | all isNumber (x:xs)         = Right $ i (x:xs)
    | x == '-' && all isNumber xs = Right $ -(i xs)
    | x == '-' || isNumber x      = asInt xs
    | otherwise                   = Left ("Not a digit: '" ++ [x] ++ "'")
      where i (y:ys) = foldl (\z a -> z * 10 + digitToInt a) 0 (y:ys)
