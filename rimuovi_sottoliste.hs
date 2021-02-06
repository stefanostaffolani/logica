--Problema: date due liste rimuovere alla prima lista tutte le sottoliste uguali alla seconda. (assumere che le sottoliste non si sovrappongono)

append :: t -> [t] -> [t]
append n [] = n:[]
append n (x:l) = x : append n l

conc :: [a] -> [a] -> [a]
conc l [] = l
conc l (x:xs) = x : conc l xs

m :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
m tmp tmp2 l1 [] res = m [] tmp2 l1 tmp2 res
m tmp tmp2 [] l2 res = conc tmp res
m tmp tmp2 (x:l) (y:ll) res = if x == y then m (append x tmp) tmp2 l ll res 
                            else m [] tmp2 l tmp2 (append x (conc res tmp))

solve_m :: [Int] -> [Int] -> [Int]
solve_m l ll = m [] ll l ll []
