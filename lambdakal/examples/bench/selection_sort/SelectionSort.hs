module SelectionSort where

import System.Random (randomRs, mkStdGen)
import Data.List (delete)
import Data.List (foldl1')

randomArray :: Int -> [Int]
randomArray n = take n $ randomRs (1, 1000000) (mkStdGen 0)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = minElem : selectionSort (delete minElem xs)
  where
    -- use foldl' for strict min search
    minElem = foldl1' min xs

main :: IO ()
main = do
  let sortedList = selectionSort (randomArray 5000)
  print sortedList

