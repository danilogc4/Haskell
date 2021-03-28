linearizar :: [[Integer]] -> [Integer]
linearizar [] = []
linearizar (a:x) = a ++ linearizar x
