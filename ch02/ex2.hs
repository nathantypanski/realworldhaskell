-- Write a function, lastButOne, that returns the element before the last.

lastButOne :: [a] -> a
lastButOne xs = if null $ tail $ tail  xs
                then last $ take 1 xs
                else lastButOne $ tail xs
