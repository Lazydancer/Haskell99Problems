{-
 8 Problem 18

(**) Extract a slice from a list.

Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

Example:

* (slice '(a b c d e f g h i k) 3 7)
(C D E F G)

Example in Haskell:

*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
-}

slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice _ 1 2 = []
slice (x:xs) 1 k = x : slice xs 1 (k-1) 
slice (x:xs) i k = slice xs (i-1) k
