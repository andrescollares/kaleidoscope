module RecursiveFib where

recursiveFib :: Int -> Int
recursiveFib 0 = 0
recursiveFib 1 = 1
recursiveFib n = recursiveFib (n - 1) + recursiveFib (n - 2)

main :: IO ()
main = do
  let fibs = map recursiveFib [0..10]
  print fibs

