{-
 1 Problem 21

Insert an element at a given position into a list.

Example:

* (insert-at 'alfa '(a b c d) 2)
(A ALFA B C D)

Example in Haskell:

P21> insertAt 'X' "abcd" 2
"aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt y xs     1 = y:xs
insertAt y (x:xs) n = x:insertAt y xs (n-1)
