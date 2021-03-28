fatores :: Integer -> [Integer]
fatores n =  [ i | i <- [2..n `div` 2], n `mod` i == 0 ] ++ [n]

primo :: Integer -> Bool
primo n
    | n == 2 = True
    | n < 2 = False
    | fatores n == [n] = True
    | otherwise = False