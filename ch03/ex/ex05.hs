-- Write a function that determines if its input is a palindrome.

palin :: String -> Bool
palin s = reverse ( strip s ) == strip s
    where
        strip :: String -> String
        strip (x:xs)
            | x == ' '  = strip xs
            | otherwise = x : strip xs
        strip x = if x == " "  then "" else x
