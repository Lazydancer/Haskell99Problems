{-
9 Problem 19

(**) Rotate a list N places to the left.

Hint: Use the predefined functions length and (++).

Examples:

* (rotate '(a b c d e f g h) 3)
(D E F G H A B C)

* (rotate '(a b c d e f g h) -2)
(G H A B C D E F)

Examples in Haskell:

*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
-}

rotate :: [a] -> Int -> [a]
rotate x n | n == 0 = x 
           | n > 0  = rotate (tail x ++ [head x]) (n-1)
           | n < 0  = rotate (last x : init x) (n+1)
