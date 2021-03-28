fatores :: Integer -> [Integer]
fatores n =  [ i | i <- [1..n `div` 2], n `mod` i == 0 ] ++ [n]

somatorio :: [Integer] -> Integer
somatorio [] = 0
somatorio (a:x) = a + somatorio x

perfeito :: Integer -> Bool
perfeito n
    | n == somatorio (fatores n) - n = True
    | otherwise = False