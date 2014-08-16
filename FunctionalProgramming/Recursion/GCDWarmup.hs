module GCDWarmup where

import Text.Printf (printf)


-- Useful functions
factorial :: Integral a => a -> a
factorial n 
  | n < 0  = (-1)
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)
                

-- Functional Programming: Warmups in Recursion - Computing the GCD
gcd' :: Integral a => a -> a -> a
gcd' n m 
  | keepGoing && n > m = gcd' (mod n m) m
  | keepGoing && n < m = gcd' n $ mod m n
  | otherwise = max n m
    where keepGoing = n /= 0 && m /= 0

