troca :: Integer -> Integer -> Integer -> [Integer]
troca pos_atual valor_a_ser_trocado valor
    | pos_atual == valor_a_ser_trocado = [valor]
    | pos_atual /= valor_a_ser_trocado = [pos_atual]


substituir :: Integer -> Integer -> [Integer] -> [Integer]
substituir a b [] = []
substituir a b (c:x) = troca c a b ++ substituir a b x