--Problema: data una lista di interi ritornare la lista delle posizioni dei massimi. (Assumere inizio lista da 1)

massimo :: Int -> [Int] -> Int
massimo n [] = n
massimo n (x:l) = if x > n then massimo x l else massimo n l

occ :: Int -> Int -> [Int] -> [Int]

occ p v [] = []
occ p v (x:l) = if x==v then p : (occ (p+1) v l) else occ (p+1) v l

f :: [Int] -> [Int]
f l = occ 1 (massimo 0 l) l 
