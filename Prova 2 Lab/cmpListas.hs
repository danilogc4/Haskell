data ListaT = Sublista | Superlista | Identica | Invalida
    deriving (Show, Eq)

tamanhoLista :: [Int] -> Int -> Int
tamanhoLista [] b = b
tamanhoLista (a:x) b = tamanhoLista x (b+1)

verificaSub :: [Int] -> [Int] -> Int -> Int -> [Int] -> Bool
verificaSub [] _ 0 _ _ = True
verificaSub _ [] 0 _ _ = True
verificaSub _ [] _ _ _ = False
verificaSub (a:x) (b:y) tamAux tamOriginal listaOriginal
    | a == b = verificaSub x y (tamAux - 1) tamOriginal listaOriginal
    | otherwise = verificaSub listaOriginal y tamOriginal tamOriginal listaOriginal

cmpListas :: [Int] -> [Int] -> ListaT
cmpListas a b
    | a == b = Identica
    | verificaSub a b (tamanhoLista a 0) (tamanhoLista a 0) a == True = Sublista
    | verificaSub b a (tamanhoLista b 0) (tamanhoLista b 0) b == True = Superlista
    | otherwise = Invalida