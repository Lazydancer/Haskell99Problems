{-
 10 Problem 20

(*) Remove the K'th element from a list.

Example in Prolog:

?- remove_at(X,[a,b,c,d],2,R).
X = b
R = [a,c,d]

Example in Lisp:

* (remove-at '(a b c d) 2)
(A C D)

(Note that this only returns the residue list, while the Prolog version also returns the deleted element.)

Example in Haskell:

*Main> removeAt 2 "abcd"
('b',"acd")
-}

removeAt :: [a] -> Int -> (a,[a])
removeAt x n = let a = removeAt' x n in (last a, init a)
  where 
    removeAt' (x:xs) 0 = xs ++ [x]
    removeAt' (x:xs) n = x : removeAt' xs (n-1)
