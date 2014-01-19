mean :: (Integral a, Fractional b) => [a] -> b
mean x = let l = fromIntegral $ length x
             s = fromIntegral $ sum x
         in s / l

