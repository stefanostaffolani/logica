--Problema: data una lista calcolare il numero massimo di occorrenze di fila.

seq_occ :: Int -> [Int] -> Int
seq_occ n [] = 1
seq_occ n (x:l) = if x == n then  1 + seq_occ n l else 1

f_helper :: Int -> [Int] -> Int
f_helper m [] = m
f_helper m (x:l) = if seq_occ x l > m then f_helper (seq_occ x l) l else f_helper m l

f :: [Int] -> Int
f l = f_helper 0 l
