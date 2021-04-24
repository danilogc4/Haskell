fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

geraLista :: Int -> [Int]
geraLista 0 = []
geraLista n = [] ++ [fib n] ++ geraLista (n-1)

tamanhoLista :: [Int] -> Int -> Int
tamanhoLista [] b = b
tamanhoLista (a:x) b = tamanhoLista x (b+1)


fibonacci :: [Int] -> Bool
fibonacci a
    | a == [] = False
    | a == [1] = False
    | a == reverse (geraLista (tamanhoLista a 0)) = True
    | otherwise = False