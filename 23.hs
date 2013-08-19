{-

3 Problem 23

Extract a given number of randomly selected elements from a list.

Example:

* (rnd-select '(a b c d e f g h) 3)
(E D A)

Example in Haskell:

Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
-}
import System.Random

rndSelect :: [a] -> Int -> IO [a]
rndSelect xs n
  | n /= 0 = do gen <- getStdGen
                let (a,_) = randomR (0,length xs) gen 
                let b     = dropElem xs a  
                rndSelect b (n-1) 
  | otherwise = return xs 

dropElem :: [a] -> Int -> [a]
dropElem (x:xs) 0 = xs
dropElem (x:xs) n = x : dropElem xs (n-1) 
