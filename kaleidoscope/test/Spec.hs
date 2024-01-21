{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Run this test with the command:
-- stack build Testing:test:unit-test

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.AST
import Data.String

import ParserH
import JIT
-- import Emit
import IRBuilder
import LLVM.IRBuilder.Module (buildModule)
import Codegen
import Control.Monad (void)

import Debug.Trace

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, jitTests]

assertAST :: String -> String -> Assertion
assertAST code ast = do
  let res = parseToplevel code
  case res of
      Left err -> assertFailure ("***\nTEST PARSE ERROR:\n" ++ show err ++ "\n***")
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
  -- , "fib_iterative"
  , "fib"
  , "hello_world"
  , "id"
  , "pow_operator"
  , "pow"
  , "sequence_operator"
  , "sub"
  , "unary_minus"
  , "let_in"
  , "let_in_multiple"
  ]


testProgramJIT :: String -> Double -> IO ()
testProgramJIT programName expectedValue = do
  source <- readFile $ "./test/programs/" ++ programName ++ ".k"
  let res = parseToplevel source
  case res of
    Left err -> assertFailure ("***\nTEST PARSE ERROR:\n" ++ show err ++ "\n***")
    Right expressions -> do
      result <- runJIT unoptimizedAst -- expressions
      result @?= expectedValue
      where
        modlState = mapM genTopLevel expressions
        unoptimizedAst = buildModule "kaleidoscope" modlState

jitTests :: TestTree
jitTests = testGroup "JIT Tests" $ map (\(s, expectedValue) -> testCase s $ do testProgramJIT s expectedValue) [
  ("add", 19)
  , ("add_sub", 15)
  -- , ("factorial_print", 40320.0) fails bacuase it uses external :(
  , ("factorial", 120)
  -- , ("fib_iterative", 0)
  , ("fib", 8)
  -- , ("hello_world", 0)
  , ("id", 900)
  , ("pow_operator", 32)
  , ("pow", 64)
  -- , ("sequence_operator", 0)
  , ("sub", -1)
  , ("unary_minus", -40)
  , ("let_in", 12.56)
  , ("let_in_multiple", 6)
  ]

-- TODO: compare llvm output
-- TODO: REPL features tests
