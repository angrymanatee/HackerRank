module SierpinskiTriangle where


class MatrixArray a where
  (<=>) :: a -> a -> a
  (<^>) :: a -> a -> a


data Matrix a = Matrix {
  fromMatrix :: [[a]]}
             deriving (Show, Eq)


instance MatrixArray (Matrix a) where
  (<=>) a b = Matrix $ concat' aStr bStr
    where aStr = fromMatrix a
          bStr = fromMatrix b
          concat' :: [[a]] -> [[a]] -> [[a]]
          concat' as [] = as
          concat' [] bs = bs
          concat' (a:as) (b:bs) = (a ++ b) : concat' as bs 
  (<^>) a b = Matrix . concat . map (fromMatrix) $ [a,b]


-- Don't know how to make this act differently for Strings...
putASCIIMatrix :: Matrix Char -> IO ()
putASCIIMatrix = mapM_ putStrLn . fromMatrix


putMatrix :: (Show a) => Matrix a -> IO ()
putMatrix = mapM_ (putStrLn . show) . fromMatrix

-- Create a 'box' that's height x width of replicated fills
box :: a -> Int -> Int -> [[a]]
box fill height width = replicate height . replicate width $ fill

-- Create an 'equilateral triangle' that's height x (2*height - 1)
triangle :: a -> a -> Int -> [[a]]
triangle empty fill height = map (triangleLayer empty fill width) [1,3..width]
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

-- Creates Sierpinski Matrix of given height and recursion
sierpinski :: a -> a -> Int -> Int -> Matrix a
sierpinski empty fill height nFrac = 
  if nFrac == 0 || height `mod` 2 /= 0
  then Matrix $ triangle empty fill height
  else let nextHeight = height `div` 2
           nextS = sierpinski empty fill (height `div` 2) (nFrac - 1) 
           bi = Matrix . box empty nextHeight
       in (bi nextHeight <=> nextS <=> bi nextHeight) 
          <^> (nextS <=> bi 1 <=> nextS)
          
sierpinskiMain :: IO ()
sierpinskiMain = do 
  input <- getLine
  putASCIIMatrix . sierpinski '_' '1' 32 . (read::String->Int) $ input