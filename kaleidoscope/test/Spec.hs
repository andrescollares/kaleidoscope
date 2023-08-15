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
parserTests = testGroup "Parser Tests" $ map (\s -> testCase s $ do testProgram s) [
  "add"
  , "add_sub"
  , "factorial_print"
  , "factorial"
  , "fib_iterative"
  , "fib"
  , "hello_world"
  , "id"
  , "pow_operator"
  , "pow"
  , "sequence_operator"
  , "sub"
  , "unary_minus"
  , "global"
  , "var_in"]

-- TODO: add RunJIT examples to compare llvm output
-- TODO: add RunJIT examples to compare program result
