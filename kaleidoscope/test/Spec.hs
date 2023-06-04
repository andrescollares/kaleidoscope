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

testProgram :: String -> IO ()
testProgram programName = do
  source <- readFile $ "./test/programs/" ++ programName ++ ".k"
  expectedAST <- readFile $ "./test/parsed/" ++ programName
  assertAST source expectedAST

parserTests :: TestTree
parserTests = testGroup "Parser Tests"
  [
    testCase "add" $ do testProgram "add"
    , testCase "add_sub" $ do testProgram "add_sub"
    , testCase "factorial_print" $ do testProgram "factorial_print"
    , testCase "factorial" $ do testProgram "factorial"
    , testCase "fib_iterative" $ do testProgram "fib_iterative"
    , testCase "fib" $ do testProgram "fib"
    , testCase "hello_world" $ do testProgram "hello_world"
    , testCase "id" $ do testProgram "id"
    , testCase "pow_operator" $ do testProgram "pow_operator"
    , testCase "pow" $ do testProgram "pow"
    , testCase "sequence_operator" $ do testProgram "sequence_operator"
    , testCase "sub" $ do testProgram "sub"
    , testCase "unary_minus" $ do testProgram "unary_minus"
    , testCase "var_in" $ do testProgram "var_in"
  ]

-- TODO: add RunJIT examples to compare llvm output
-- TODO: add RunJIT examples to compare program result
