module StringOPermute where

import System.IO
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.Builder as B
import qualified Data.Monoid as M
  

-- Reads the first number that the program needs. Ignores rest of string
readFirstNum :: Maybe (Int, L.ByteString) -> Int 
readFirstNum x = case x of
  Nothing -> (-1)
  Just (n,_) -> n


-- Swaps elements in the Bytestring and creates a builder
swapElemsB :: L.ByteString -> B.Builder
swapElemsB str = if L.length str > 1
                 then let first = B.char8 . L.head $ str
                          second = B.char8 . L.head . L.tail $ str
                          rest = L.drop 2 str
                      in second M.<> first M.<> swapElemsB rest
                 else B.lazyByteString str M.<> B.char8 '\n'


-- Main function for permuting the string
stringOPermuteMain :: IO ()
stringOPermuteMain = do
  allData <- L.getContents
  let chunks = L.lines allData
      numTests = readFirstNum . L.readInt . head $ chunks
      tests = tail chunks
  runTests numTests tests


runTests :: Int -> [L.ByteString] -> IO ()
runTests num tests = if num > 0
                     then do 
                       B.hPutBuilder stdout . swapElemsB . head $ tests
                       runTests (pred num) (tail tests)
                     else return ()

