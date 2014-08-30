module FilterElements where

import Data.List
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

addToDict :: Eq a => [(a, Int)] -> a -> [(a, Int)]
addToDict dict a = case dict of
  [] -> [(a, 1)]
  d@(key, num):ds -> if a == key
                     then (a, succ num) : ds
                     else d : addToDict ds a 

countElems :: Eq a => [a] -> [(a, Int)]
countElems = foldl addToDict []

elemsWithMoreThan :: Eq a => Int -> [a] -> [a]
elemsWithMoreThan num = filterElems num . countElems
  where filterElems :: Eq a => Int -> [(a, Int)] -> [a]
        filterElems num = map fst . filter ((>=num) . snd)

getNums :: BC.ByteString -> [Int]
getNums = internal . BC.readInt . BC.dropWhile (==' ')
  where internal x = case x of
          Just (x, xs) -> x : getNums xs
          Nothing -> []


filterMain :: IO ()
filterMain = do
  numTests <- BC.getLine
  performTests . head . getNums $ numTests

performTests :: Int -> IO ()
performTests n 
  | n <= 0 = return ()
  | otherwise = do rawLen <- BC.getLine
                   rawVec <- BC.getLine
                   let size = last . getNums $ rawLen
                       vec = getNums $ rawVec
                   BC.putStrLn . BC.intercalate (BC.pack " ") . map (BC.pack . show) . test size $ vec
                   performTests $ pred n


test :: Int -> [Int] -> [Int]
test num = (\a -> if null a then [(-1)] else a) . elemsWithMoreThan num 

main :: IO ()
main = filterMain
