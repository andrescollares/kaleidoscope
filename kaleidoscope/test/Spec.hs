module Main where

-- Run this test with the command:
-- stack build Testing:test:unit-test

import Test.Tasty
import Test.Tasty.HUnit

import ParserH

import Debug.Trace

main :: IO ()
main = defaultMain parserTests

assertAST :: String -> String -> Assertion
assertAST code ast = do
  let res = parseToplevel code
  case res of
      Left err -> 1 @?= 0
      Right ex -> do
        expressions <- mapM_ print ex
        show ex @?= ast

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [
    testCase "add" $ assertAST 
      "def add(x y) x + y; add(9, 10);"
      "[Function \"add\" [\"x\",\"y\"] (BinOp \"+\" (Var \"x\") (Var \"y\")),Call \"add\" [Float 9.0,Float 10.0]]"
    , testCase "sub" $ assertAST 
      "def sub(x y) x - y; sub(9, 10);"
      "[Function \"sub\" [\"x\",\"y\"] (BinOp \"-\" (Var \"x\") (Var \"y\")),Call \"sub\" [Float 9.0,Float 10.0]]"
    , testCase "add_sub" $ assertAST 
      "def add(x y) x + y;\
       \ def sub(x y) x - y;\
       \ add(10, sub(6, 1));"
      "[Function \"add\" [\"x\",\"y\"] (BinOp \"+\" (Var \"x\") (Var \"y\")),Function \"sub\" [\"x\",\"y\"] (BinOp \"-\" (Var \"x\") (Var \"y\")),Call \"add\" [Float 10.0,Call \"sub\" [Float 6.0,Float 1.0]]]"
    , testCase "var_in" $ assertAST
      "def binary : 1 (x y) y;\

      \def circlearea(r) \
          \var pi = 3.14 in \
          \pi * r * r;\

      \var dos = 10000 in \
          \dos + circlearea(2) :\
          \circlearea(dos);"
      "[BinaryDef \":\" [\"x\",\"y\"] (Var \"y\"),Function \"circlearea\" [\"r\"] (Let \"pi\" (Float 3.14) (BinOp \"*\" (BinOp \"*\" (Var \"pi\") (Var \"r\")) (Var \"r\"))),Let \"dos\" (Float 10000.0) (BinOp \":\" (BinOp \"+\" (Var \"dos\") (Call \"circlearea\" [Float 2.0])) (Call \"circlearea\" [Var \"dos\"]))]"
  ]

-- TODO: add RunJIT examples to compare llvm output
-- TODO: add RunJIT examples to compare program result
