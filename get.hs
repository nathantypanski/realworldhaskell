filename = "discoveraq-CH2O_P3B_20110701_R1_1Sec.ict"

get :: (Num b) => b -> [a] -> [a]
get _ [] = []
get n (x:xs) = get (n-1) xs
get 1 (x:[]) = x
