module HackerRank where

import Text.Printf (printf)
import Data.List

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = map ((*(1/ fromIntegral freq)) . sum) [vals, map (area) vals]
  where 
    freq = 1000 :: Int
    tRange = floatingRange freq l r
    vals = map (f) tRange
    
    floatingRange :: (Integral a, Fractional b) => a -> a -> a -> [b]
    floatingRange fr l r = map (intDiv) $ [l*fr..r*fr]
      where intDiv = (/ fromIntegral fr) . fromIntegral
    
    area :: Floating a => a -> a
    area = (*pi) . (^2) 
        
    f :: Floating a => a -> a
    f t = sum . zipWith (*) aFl . map (t**) $ bFl
      where aFl = map (fromIntegral) a
            bFl = map (fromIntegral) b


gcd' :: Integral a => a -> a -> a
gcd' n m 
  | keepGoing && n > m = gcd' (mod n m) m
  | keepGoing && n < m = gcd' n $ mod m n
  | otherwise = max n m
    where keepGoing = n /= 0 && m /= 0


factorial :: Integral a => a -> a
factorial n 
  | n < 0  = (-1)
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)


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
