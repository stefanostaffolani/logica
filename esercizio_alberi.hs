{--
dato un albero binario scrivere la funzione f che prende in input un albero e ritorna (n,m,b) dove:
    n : numero t.c. uno dei più grandio sotto-alberi di  T ha tutte le foglie etichettate con n;
    m : numero di nodi di tale albero;
    b : 1 se le foglie di T sono tutte etichettate con n
--}

data T = Leaf Int | Node T T deriving Show

first (x:y:z:[])= x
second (x:y:z:[]) = y
third (x:y:z:[]) = z

f (Leaf n) = (n:1:1:[])
f (Node t1 t2) = if (first (f t1) == first (f t2)) && (third(f t1) * third(f t2) == 1) 
                    then (first(f t1) : (second(f t1) + second(f t2)) : (third(f (t1)) * third(f t2)) : [])
                else if second(f t1) > second(f t2)
                    then (first(f t1) : second(f t1) : 0 : []) 
                else (first(f t2) : second(f t2) : 0 : [])