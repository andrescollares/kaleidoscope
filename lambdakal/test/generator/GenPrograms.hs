module GenPrograms where

import Data.List (intercalate)
import System.Environment (getArgs)

functionFromIndex :: Int -> String
functionFromIndex i = "def f_" ++ show i ++ "(int x) -> int: x;"

letInFromIndex :: Int -> String
letInFromIndex i = intercalate (show i) ["let int var_", " = f_", "(", ") in"]

letInsString :: Int -> String
letInsString iterations = intercalate "\n" $ map letInFromIndex [1 .. iterations]

functionDefsString :: Int -> String
functionDefsString iterations = intercalate "\n" $ map functionFromIndex [1 .. iterations]

operationsString :: Int -> String
operationsString iterations = intercalate " + " $ map (\i -> "var_" ++ show i) [1 .. iterations]

programString :: Int -> String
programString iterations = functionDefsString iterations ++ "\n" ++ letInsString iterations ++ "\n" ++ operationsString iterations ++ ";"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> writeFile "/lambdakal/test/generator/out.k" $ programString 100
    [iterations] -> writeFile "/lambdakal/test/generator/out.k" $ programString (read iterations)
    _ -> putStrLn "Usage: GenPrograms [iterations]"