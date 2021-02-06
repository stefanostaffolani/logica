--Problema: data una lista di liste ritornare la lista contenente i singoli elementi che compaiono in ogni lista della lista.

append :: t -> [t] -> [t]
append n [] = n : []
append n (x:l) = x : append n l

search :: Eq t => t -> [t] -> Bool
search n [] = False
search n (x:l) = x == n || search n l

search_all :: Eq t => t -> [[t]] -> Bool
search_all n [] = True
search_all n (x:l) = search n x && search_all n l 

testa::[[Int]]->[Int]
testa [] = []
testa (x:l) = x

f_aux :: Eq t => [t] -> [t] -> [[t]] -> [t]
f_aux ll [] l_v = ll
f_aux ll (x:xs) l_v = if search_all x l_v then f_aux (append x ll) xs l_v else f_aux ll xs l_v

f :: [[Int]] -> [Int]
f l = f_aux [] (testa l) l
