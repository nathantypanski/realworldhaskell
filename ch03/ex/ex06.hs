-- Create a function that sorts a list of lists based on the length of each
-- sublist.

import Data.List

sortLists :: [[a]] -> [[a]]
sortLists l = sortBy (\ a b -> compare (length a) (length b)) l
