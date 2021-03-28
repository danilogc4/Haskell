somatorio :: [Integer] -> Integer
somatorio [] = 0
somatorio (a:x) = a + somatorio x


inverte :: [Integer] -> [Integer] -> [Integer]
inverte (a:x) m
    | x == [] = m ++ [a]
    | otherwise = inverte x m ++ [a]


somador :: [Integer] -> [Integer] -> [Integer]
somador (a:x) b
    | x == [] = [a] ++ b
    | otherwise = somador x ([a + somatorio x] ++ b)


somaParciais :: [Integer] -> [Integer]
somaParciais [] = []
somaParciais a = somador (inverte a []) []