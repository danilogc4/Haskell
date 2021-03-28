existe :: Integer -> [Integer] -> Bool
existe n (a:x)
    | x == [] = n == a
    | n == a = True
    | otherwise = existe n x


disjuntas :: [Integer] -> [Integer] -> Bool
disjuntas (a:x) m
    | x == [] = False == existe a m
    | existe a m == True = False
    | otherwise = disjuntas x m
