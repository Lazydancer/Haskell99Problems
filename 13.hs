{-
 3 Problem 13

(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:

* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

Example in Haskell:

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']

I had to cheat on this one, here is the given solution
-}

data Item a =  Multiple Int a | Single a deriving (Show) 

encode' :: [a] -> [(Int,a)]
encode' = foldr helper []
  where 
    helper x [] = [(1,x)]
    helper x (y@(a,b):ys)
      | x == b    = (1+a,x):ys
      | otherwise = (1,x):y:ys 

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect = map encodeHelper . encode'
    where
      encodeHelper (1,x) = Single x
      encodeHelper (n,x) = Multiple n x
