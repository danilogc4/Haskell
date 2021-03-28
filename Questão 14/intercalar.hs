linearizar :: [[Integer]] -> [Integer]
linearizar [] = []
linearizar (a:x) = a ++ linearizar x

qsort :: [Integer] -> [Integer]
qsort [] = []
qsort (a:x) = qsort[ b | b <- x, b <= a]++ [a] ++qsort[b | b <- x, b > a]

intercalar :: [[Integer]] -> [Integer]
intercalar [] = []
intercalar (a:x) =  qsort (linearizar ([a]++x))
