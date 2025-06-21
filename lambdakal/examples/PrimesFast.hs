module PrimesFast where

primesUntil :: Int -> [Int]
primesUntil n = sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = sieve [x | x <- xs, x `mod` p /= 0] ++ [p]

main :: IO ()
main = do
  let primes = primesUntil 30000
  print primes

