{-
5 Problem 25

Generate a random permutation of the elements of a list.

Example:

* (rnd-permu '(a b c d e f))
(B A D C E F)

Example in Haskell:

Prelude>rnd_permu "abcdef"
Prelude>"badcef"
-}

import System.Random
import Data.List (nub)

rndPermu :: (Eq a) => [a] -> IO [a]
rndPermu xs = do 
  gen <- getStdGen 
  return $ take (length xs) $ nub [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]
