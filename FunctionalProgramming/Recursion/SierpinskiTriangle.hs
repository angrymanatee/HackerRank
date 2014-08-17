module SierpinskiTriangle where

import Common


-- Creates Sierpinski Matrix of given height and recursion
sierpinski :: a -> a -> Int -> Int -> Matrix a
sierpinski empty fill height nFrac = 
  if nFrac == 0 || height `mod` 2 /= 0
  then triangle empty fill height
  else let nextHeight = height `div` 2
           nextS = sierpinski empty fill (height `div` 2) (nFrac - 1) 
           centerline = box empty nextHeight 1
       in padHorz empty nextHeight nextS <^> (nextS <=> centerline <=> nextS)
          
sierpinskiMain :: IO ()
sierpinskiMain = getLine >>= putASCIIMatrix . sierpinski '_' '1' 32 . read

