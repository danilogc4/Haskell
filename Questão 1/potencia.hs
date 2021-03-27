potencia :: Integer -> Integer -> Integer
potencia p 0 = 1
potencia p q = p * potencia p (q - 1)
