module Sum where

main = print $ sum1 [1.0..100000000.0]
sum1 [] = 0
sum1 (x : xs) = x + sum1 xs