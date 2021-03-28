pegaInicio :: Integer -> [Integer] -> [Integer]
pegaInicio c (a:x)
    | c == 1 = [a]
    | otherwise = [a]++(pegaInicio (c-1) x)


tamanho :: [Integer] -> Integer
tamanho [] = 0
tamanho (a:x) = 1 + tamanho x

removerFim :: Integer -> [Integer] -> [Integer]
removerFim n (a:x)
    | n == 0 = [a]++x
    | otherwise = pegaInicio ((tamanho ([a]++x))-n) ([a]++x)
