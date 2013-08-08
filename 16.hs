{-
 6 Problem 16

(**) Drop every N'th element from a list.

Example:

* (drop '(a b c d e f g h i k) 3)
(A B D E G H K)

Example in Haskell:

*Main> dropEvery "abcdefghik" 3
"abdeghk"
-}

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper xs n 
  where helper [] _ = []
        helper (x:xs) 1 = helper xs n
        helper (x:xs) a = x : helper xs (a-1)
