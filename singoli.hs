--Problema: data una lista ritornare la lista dei singoli elementi che compaiono

app :: t -> [t] -> [t]
app n [] = n:[]
app n (x:l) = x : app n l

search :: Eq t => t -> [t] -> Bool
search n [] = False
search n (x:l) = x == n || search n l

fh :: Eq t => [t] -> [t] -> [t]
fh lista_aux [] = lista_aux
fh lista_aux (x:l) = if search x lista_aux then fh lista_aux l else fh (app x lista_aux) l

f :: Eq t => [t] -> [t]
f l = fh [] l
