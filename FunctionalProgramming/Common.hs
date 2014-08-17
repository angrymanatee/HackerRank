module Common where


-- Useful functions
factorial :: Integral a => a -> a
factorial n 
  | n < 0  = (-1)
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)
