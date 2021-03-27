somaSeImpar :: Integer -> Integer
somaSeImpar m
    | (m `mod` 2 == 0) = 0
    | (m `mod` 2 == 1) = m

somaImpares :: [Integer] -> Integer
somaImpares [] = 0
somaImpares (a:x) = somaSeImpar a + somaImpares x
