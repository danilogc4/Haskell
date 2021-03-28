retiraInicio :: Integer -> [Integer] -> [Integer]
retiraInicio b (a:x)
    | b == 1 = x
    | otherwise = retiraInicio (b-1) x


final :: Integer -> [Integer] -> [Integer]
final c (a:x)
    | c == 1 = [a]
    | otherwise = [a]++(final (c-1) x)


shift :: Integer -> [Integer] -> [Integer]
shift n (a:x)
    | n == 0 = [a]++x
    | otherwise = retiraInicio n ([a]++x++final n ([a]++x)) -- [a]++x++final n ([a]++x) = [] + final
    
