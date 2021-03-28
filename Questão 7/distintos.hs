existe :: Integer -> [Integer] -> Bool
existe n (a:x)
    | x == [] = False
    | n == a = True
    | otherwise = existe n x


distintos :: [Integer] -> Bool
distintos (a:x)
    | x == [] = True
    | existe a x == True = False 
    | otherwise = distintos x

