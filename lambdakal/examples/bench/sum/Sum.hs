module Sum where

sum' :: [Double] -> Double
sum' [] = 0
sum' (x:xs) = x + sum' xs



main :: IO ()
main = do
  let numbers = [1.0..1000000.0]
  print (sum' numbers)