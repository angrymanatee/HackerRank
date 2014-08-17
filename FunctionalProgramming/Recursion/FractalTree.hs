module FractalTree where

import Common


tree :: a -> a -> Int -> Matrix a
tree empty fill height = (padR empty 1 branch <=> flipLR branch)
                         <^> (padLR empty partHeight stem)
  where partHeight = height `div` 2
        branch = diagonalMatrix empty fill partHeight
        stem = box fill partHeight 1

fractalTree :: a -> a -> Int -> Int -> Matrix a
fractalTree empty fill height nFrac
  | nFrac == 0 || height == 2  = tree empty fill height
  | otherwise                  = top <^> bottom
  where nextTree = fractalTree empty fill (height `div` 2) (pred nFrac)
        thisTree = tree empty fill height
        widthBetweenTrees = matWidth thisTree - matWidth nextTree - 1
        padWidthBottom = (matWidth top - matWidth thisTree) `div` 2
        top = padR empty widthBetweenTrees nextTree <=> nextTree
        bottom =  padLR empty padWidthBottom thisTree

minDimsCenter :: a -> Int -> Int -> Matrix a -> Matrix a
minDimsCenter fill height width mat = padTUpto . padLRUpto $ mat
  where widthToPad = max 0 $ width - matWidth mat
        heightToPad = max 0 $ height - matHeight mat
        extraWidthToPad = widthToPad `mod` 2
        padLRUpto = padR fill extraWidthToPad . padLR fill (widthToPad `div` 2)
        padTUpto = padT fill heightToPad

fractalTreeMain :: IO ()
fractalTreeMain = getLine >>= putASCIIMatrix . dimsReq . treeFunc . pred . read
  where empty = '_'
        fill = '1'
        dimsReq = minDimsCenter empty 63 100
        treeFunc = fractalTree empty fill 32