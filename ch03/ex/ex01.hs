-- Write a function that computes the number of elements in
-- a list. To test it, ensure that it gives the same answers
-- as the standard `length` function.

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

-- Monadic test functions, for play.

testLength :: [a] -> Maybe Bool
testLength x = if length x == length' x
                       then Just True
                       else Nothing

testLengths :: Maybe Bool
testLengths = do
                testLength [1..10]
                testLength [-100..10]
                testLength []
                testLength [1]

-- Normal test functions

testLengths' :: Bool
testLengths' = foldr (&&) True tests
  where tests = map testLength' [[1..10], [-100..10], [], [1]]
        testLength' x = length x == length' x
