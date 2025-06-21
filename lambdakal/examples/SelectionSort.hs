module SelectionSort where

import System.Random (randomRs, mkStdGen)

randomArray :: Int -> [Int]
randomArray n = take n $ randomRs (1, 1000000) (mkStdGen 0)

selectionSort:: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = let (minElem, rest) = findMin xs
                      in minElem : selectionSort rest
  where
    findMin :: (Ord a) => [a] -> (a, [a])
    findMin [] = error "Empty list has no minimum"
    findMin [x] = (x, [])
    findMin (x:xs) = let (minTail, rest) = findMin xs
                        in if x < minTail
                            then (x, minTail : rest)
                            else (minTail, x : rest)

main :: IO ()
main = do
  let sortedList = selectionSort (randomArray 5000)

  print sortedList

