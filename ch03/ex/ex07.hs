-- Define a function that joins a list of lists together using a separator value:

sperse :: a -> [[a]] -> [a]
sperse sep [] = []
sperse sep list = head list ++ [sep] ++ sperse sep (tail list)

intersperse :: a -> [[a]] -> [a]
intersperse sep [] = []
intersperse sep list = init $ sperse sep list
