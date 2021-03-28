ordenar :: [Integer] -> [Integer]
ordenar [] = []
ordenar (a:x) = ordenar[ s | s <- x, s <= a]++[a]++ordenar[s | s <- x, s > a]

troco :: Integer -> [Integer]
troco n
    | (n - 100) >= 0 = [100] ++ trocar (n-100)
    | (n - 50) >= 0 = [50] ++ trocar (n-50)
    | (n - 20) >= 0 = [20] ++ trocar (n-20)
    | (n - 10) >= 0 = [10] ++ trocar (n-10)
    | (n - 5) >= 0 = [5] ++ trocar (n-5)
    | (n - 1) >= 0 = [1] ++ trocar (n-1)
    | otherwise = []


trocar :: Integer -> [Integer]
trocar n
    | n <= 0 = []
    | otherwise = ordenar (troco n)
