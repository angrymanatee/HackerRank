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

diagonalMatrix :: a -> a -> Int -> Matrix a
diagonalMatrix empty fill width = 
  Matrix $ map (diagonalLayer empty fill width) [0..width-1]

diagonalLayer :: a -> a -> Int -> Int -> [a]
diagonalLayer empty fill width index =
          if index == 0
          then fill : replicate (pred width) empty
          else empty : diagonalLayer empty fill (pred width) (pred index)


-- Assumes matrix is constant width in all columns
matWidth :: Matrix a -> Int
matWidth = length . head . fromMatrix

matHeight :: Matrix a -> Int
matHeight = length . fromMatrix

-- Pad left side of matrix
padL :: a -> Int -> Matrix a -> Matrix a
padL fill padWidth mat = b <=> mat
  where b = box fill (matHeight mat) padWidth

-- Pad right side of matrix
padR :: a -> Int -> Matrix a -> Matrix a
padR fill padWidth mat = mat <=> b
  where b = box fill (matHeight mat) padWidth
        
-- Pad top of matrix
padT :: a -> Int -> Matrix a -> Matrix a
padT fill padHeight mat = b <^> mat
  where b = box fill padHeight (matWidth mat)

-- Pad bottom of matrix
padB :: a -> Int -> Matrix a -> Matrix a
padB fill padHeight mat = mat <^> b
  where b = box fill padHeight (matWidth mat)

-- Pad both left and right side of matrix (equally)
padLR :: a -> Int -> Matrix a -> Matrix a
padLR fill padWidth mat = b <=> mat <=> b
  where b = box fill (matHeight mat) padWidth

-- Pad both top and bottom of matrix
padTB :: a -> Int -> Matrix a -> Matrix a
padTB fill padHeight mat = b <^> mat <^> b
  where b = box fill padHeight (matWidth mat)


flipLR :: Matrix a -> Matrix a
flipLR = Matrix . map reverse . fromMatrix

flipUD :: Matrix a -> Matrix a
flipUD = Matrix . reverse . fromMatrix