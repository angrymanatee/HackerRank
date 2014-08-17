module Common where


-- Useful functions
factorial :: Integral a => a -> a
factorial n 
  | n < 0  = (-1)
  | n == 0 = 1
  | otherwise = n * factorial (n - 1)


-- Matrix Functions. Useful for image generation
data Matrix a = Matrix {
  fromMatrix :: [[a]]}
             deriving (Show, Eq)

(<=>) :: Matrix a -> Matrix a -> Matrix a
(<=>) a b = Matrix $ concat' aStr bStr
  where aStr = fromMatrix a
        bStr = fromMatrix b
        concat' :: [[a]] -> [[a]] -> [[a]]
        concat' as [] = as
        concat' [] bs = bs
        concat' (a:as) (b:bs) = (a ++ b) : concat' as bs 

(<^>) :: Matrix a -> Matrix a -> Matrix a
(<^>) a b = Matrix . concat . map (fromMatrix) $ [a,b]


-- Don't know how to make this act differently for Strings...
putASCIIMatrix :: Matrix Char -> IO ()
putASCIIMatrix = mapM_ putStrLn . fromMatrix


putMatrix :: (Show a) => Matrix a -> IO ()
putMatrix = mapM_ (putStrLn . show) . fromMatrix

-- Create a 'box' that's height x width of replicated fills
box :: a -> Int -> Int -> Matrix a
box fill height width = Matrix . replicate height . replicate width $ fill

-- Create an 'equilateral triangle' that's height x (2*height - 1)
triangle :: a -> a -> Int -> Matrix a
triangle empty fill height = Matrix $ 
                             map (triangleLayer empty fill width) [1,3..width]
  where width = 2*height - 1

-- A layer in a triangle. Centered fill of fillwidth in totalwidth
triangleLayer :: a -> a -> Int -> Int -> [a]
triangleLayer empty fill totalWidth fillWidth = 
  triangleLayer' empty fill emptyWidth (fillWidth - 1)
    where emptyWidth = (totalWidth - fillWidth) `div` 2
          triangleLayer' :: a -> a -> Int -> Int -> [a]
          triangleLayer' empty fill nEmpty nFill = 
            if nEmpty <= 0
            then if nFill <= 0
                 then fill : replicate emptyWidth empty
                 else fill : triangleLayer' empty fill nEmpty (nFill - 1)
            else empty : triangleLayer' empty fill (nEmpty - 1) nFill

padHorz :: a -> Int -> Matrix a -> Matrix a
padHorz fill padWidth mat = b <=> mat <=> b
  where height = length $ fromMatrix mat
        b = box fill height padWidth
