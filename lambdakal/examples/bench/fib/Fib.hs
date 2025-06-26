module Fib where

fib :: Int -> Int
fib n = if n < 3 then 1 else fib (n - 1) + fib (n - 2)

main :: IO ()
main = print $ fib 40