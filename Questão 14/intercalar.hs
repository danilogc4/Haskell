quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (a:x) = quickSort[ s | s <- x, s <= a]++[a]++quickSort[s | s <- x, s > a]

intercalar :: [Integer] -> [Integer] -> [Integer]
intercalar [] (s:x) = [s]++x
intercalar (a:c) [] = [a]++c
intercalar [] [] = []
intercalar (a:c) (s:x)
    | otherwise = quickSort (([a]++c)++([s]++x))
