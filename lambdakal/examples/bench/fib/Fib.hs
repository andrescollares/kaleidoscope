module Fib where

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = print $ fib 40