{-
2 Problem 22

Create a list containing all integers within a given range.

Example:

* (range 4 9)
(4 5 6 7 8 9)

Example in Haskell:

Prelude> range 4 9
[4,5,6,7,8,9]
-}

range :: Int -> Int -> [Int]
range a b 
  | a < b  = a:range (a + 1) b
  | a > b  = a:range (a - 1) b
  | a == b = [b]
