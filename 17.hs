{-
7 Problem 17

(*) Split a list into two parts; the length of the first part is given.

Do not use any predefined predicates.

Example:

* (split '(a b c d e f g h i k) 3)
( (A B C) (D E F G H I K))

Example in Haskell:

*Main> split "abcdefghik" 3
("abc", "defghik")

I had to cheat on this one
-}

split :: [a] -> Int -> ([a], [a])
split []         _             = ([], [])
split l@(x : xs) n | n > 0     = (x : ys, zs)
                   | otherwise = ([], l)
    where (ys,zs) = split xs (n - 1)
