-- ex04
-- two kinds of palindromes provided:

palindrome :: [x] -> [x]
palindrome x = let back = reverse x in x ++ tail back

palindrome' :: [x] -> [x]
palindrome' x = let back = reverse x in x ++ back

-- ex05

isPalindrome :: (Eq x) => [x] -> Bool
isPalindrome x = foldr (&&) True e
  where
    e = zipWith (\a b -> a == b) x y
    y = reverse x
