module AreaVolume where

-- Area Under Curves and Volume of Revolving a Curve
-- Compute the area under polynomial curve
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
