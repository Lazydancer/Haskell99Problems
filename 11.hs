{-
 1 Problem 11

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:

* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

Example in Haskell:

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Item a =  Multiple Int a | Single a deriving (Show) 

encodeModified ::Eq a => [a] -> [Item a]
encodeModified = (map func) . encode
  where func (1,b) = Single b
        func (a,b) = Multiple a b

encode :: Eq a => [a] -> [(Int,a)]
encode = (map (\x -> (length x, head x))) . pack

pack :: Eq a => [a] -> [[a]]
pack [] =  []
pack (x:xs) = foldl func [[x]] xs
  where func xs elm  
          | elm == (last (last xs)) = (init xs) ++ [((last xs) ++ [elm])]
          | otherwise               = xs ++ [[elm]]

