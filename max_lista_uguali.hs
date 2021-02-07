--Problema: data una lista di interi ritornare la lunghezza della massima sotto-lista di interi uguali 

first :: [Int] -> Int
first [] = -1
first (x:l) = x

prima_sub :: [Int] -> Int
prima_sub [] = 0
prima_sub (x:l) = if x == first l then 1 + prima_sub l else 1

max_lunga :: Int -> [Int] -> Int
max_lunga n [] = n
max_lunga n (x:l) = if (prima_sub (x:l)) > n then max_lunga (prima_sub (x:l)) l else max_lunga n l

f :: [Int] -> Int
f l = max_lunga 0 l
