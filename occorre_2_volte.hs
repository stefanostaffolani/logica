--Problema: data una lista ritornare True sse ogni elemento della lista occorre 2 volte altrimenti False

occ :: (Num p, Eq t) => t -> [t] -> p
occ n [] = 0
occ n (x:l) = (if x == n then 1 else 0) + occ n l

helper :: Eq t => [t] -> [t] -> Bool
helper [] ll = True
helper (x:l) ll = occ x ll == 2 && helper l ll

f :: Eq t => [t] -> Bool
f l = helper l l
