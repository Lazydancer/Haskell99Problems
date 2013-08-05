{-
 2 Problem 12

(**) Decode a run-length encoded list.

Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

Example in Haskell:

P12> decodeModified 
       [Multiple 4 'a',Single 'b',Multiple 2 'c',
        Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}

data Item a =  Multiple Int a | Single a deriving (Show) 

decodeModified :: [Item a] -> [a]
decodeModified = concat . map func
  where func (Single x) = [x]
        func (Multiple 1 x) = [x]
        func (Multiple n x) = x:(func (Multiple (n-1) x))
