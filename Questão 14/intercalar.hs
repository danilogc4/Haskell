quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (a:x) = quickSort[ s | s <- x, s <= a]++[a]++quickSort[s | s <- x, s > a]

intercalar :: [Integer] -> [Integer] -> [Integer]
intercalar (a:c) (s:x)
    | otherwise = quickSort (([a]++c)++([s]++x))
