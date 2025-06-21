module Primes where

divisibleAny :: Int -> Int -> Bool
divisibleAny n i
  | i < 2     = False
  | n `mod` i == 0 = True
  | otherwise = divisibleAny n (i - 1)

isPrime :: Int -> Bool
isPrime n = not (divisibleAny n (n - 1))

primesUntil :: Int -> [Int]
primesUntil n
  | n < 2     = []
  | isPrime n = n : primesUntil (n - 1)
  | otherwise = primesUntil (n - 1)

main :: IO ()
main = do
  let primes = primesUntil 10000
  print primes



