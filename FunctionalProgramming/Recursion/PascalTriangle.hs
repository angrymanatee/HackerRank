module PascalTriangle where

import Data.List
import Common


-- Pascal's Triangle
pascal :: Integral a => a -> [[a]]
pascal n = map (pascalLevel) [0..n-1]

pascalLevel :: Integral a => a -> [a]
pascalLevel = flip (pascalLevel') 0

pascalLevel' :: Integral a => a -> a -> [a]
pascalLevel' n r 
  | n > r  = pascalValue n r : pascalLevel' n (succ r)
  | n == r = pascalValue n r:[]
  | otherwise = []

pascalValue :: Integral a => a -> a -> a
pascalValue n r = factorial n `div` (factorial r * factorial (n - r))


pascalMain :: IO ()
pascalMain = do 
  line <- getLine
  mapM_ (putStrLn . convertToStr) . pascal . (read::String->Int) $ line

convertToStr :: (Integral a, Show a) => [a] -> String
convertToStr = intercalate " " . map show
