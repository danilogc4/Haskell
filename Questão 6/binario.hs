binario :: Integer -> [Integer]
binario n
    | n == 0 = [0]
    | n == 1 = [1]
    | otherwise = binario (n `div` 2) ++ [n `mod` 2]
