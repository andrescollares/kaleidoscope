{-# LANGUAGE BangPatterns #-}
module SumTailRecursive where

main = print $ sum4 [1.0..100000.0]
sum4 xs = sum4Aux 0 xs
  where
  sum4Aux ! acc [ ] = acc
  sum4Aux ! acc (x : xs) = sum4Aux (acc + x) xs