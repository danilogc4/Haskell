inverte :: [Integer] -> [Integer] -> [Integer]
inverte (a:x) m
    | x == [] = m ++ [a]
    | otherwise = inverte x m ++ [a]

palindromo :: [Integer] -> Bool
palindromo a = a == inverte a []
