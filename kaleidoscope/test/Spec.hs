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
import IRBuilder.GenModule
import LLVM.IRBuilder.Module (buildModule)
import Control.Monad (void)
import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as ASTType

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
  -- "add"
  -- , "add_sub"
  -- , "factorial_print"
  -- , "factorial"
  -- -- , "fib_iterative"
  -- , "fib"
  -- , "hello_world"
  -- , "id"
  -- , "pow_operator"
  -- , "pow"
  -- , "sequence_operator"
  -- , "sub"
  -- , "unary_minus"
  -- , "let_in"
  -- , "let_in_multiple"
  ]


testProgramJIT :: String -> String -> AST.Type -> IO ()
testProgramJIT programName expectedValue returnType = do
  source <- readFile $ "./test/programs/" ++ programName ++ ".k"
  let res = parseToplevel source
  case res of
    Left err -> assertFailure ("***\nTEST PARSE ERROR:\n" ++ show err ++ "\n***")
    Right expressions -> do
      result <- runJIT unoptimizedAst returnType -- expressions
      result @?= expectedValue
      where
        modlState = mapM genTopLevel expressions
        unoptimizedAst = buildModule "kaleidoscope" modlState

jitTests :: TestTree
jitTests = testGroup "JIT Tests" $ map (\(s, expectedValue, returnType) -> testCase s $ do testProgramJIT s expectedValue returnType) [
  ("const_float", "10.0", ASTType.double)
  , ("const_int_float", "6.0", ASTType.double)
  , ("const_int_int", "16", ASTType.i32)
  , ("const_tuple_int", "Tuple (9, ...)", ASTType.StructureType False [ASTType.i32, ASTType.i32])
  , ("const_tuple_float", "Tuple (7.2, ...)", ASTType.StructureType False [ASTType.double, ASTType.double])
  , ("const_tuple_bool", "Tuple (true, ...)", ASTType.StructureType False [ASTType.i1, ASTType.i1])
  , ("add_float_float", "2.0", ASTType.double)
  , ("add_float_int", "2.0", ASTType.double)
  , ("add_int_float", "2.0", ASTType.double)
  , ("add_int_int", "2", ASTType.i32)
  , ("div_float_int", "2.5", ASTType.double)
  , ("div_int_int", "2", ASTType.i32)
  , ("and_true_true", "true", ASTType.i1)
  , ("and_true_false", "false", ASTType.i1)
  , ("and_false_true", "false", ASTType.i1)
  , ("and_false_false", "false", ASTType.i1)
  , ("or_true_true", "true", ASTType.i1)
  , ("or_true_false", "true", ASTType.i1)
  , ("or_false_true", "true", ASTType.i1)
  , ("or_false_false", "false", ASTType.i1)
  , ("not_true", "false", ASTType.i1)
  , ("not_false", "true", ASTType.i1)
  , ("fn_add_int_int", "3", ASTType.i32)
  , ("fn_add_float_int", "3.0", ASTType.double)
  , ("fn_add_float_float", "3.0", ASTType.double)
  , ("fn_div_int_int", "2.5", ASTType.double)
  , ("fn_nested", "9", ASTType.i32)
  , ("fn_int_float", "3.5", ASTType.double)
  , ("if_int", "10", ASTType.i32)
  , ("if_float", "2.5", ASTType.double)
  , ("let_in_int", "5", ASTType.i32)
  , ("let_in_float", "5.0", ASTType.double)
  , ("let_in_multiple", "6", ASTType.i32)
  , ("let_in_nested", "6.0", ASTType.double)
  , ("recursive_sum", "55", ASTType.i32)
  , ("recursive_fib", "8", ASTType.i32)
  , ("redefinition_function", "1050", ASTType.i32)
  , ("redefinition_function_recursive", "55", ASTType.i32)
  , ("fst_int", "9", ASTType.i32)
  , ("fst_float", "9.99", ASTType.double)
  , ("fst_tuple", "Tuple (1, ...)", ASTType.StructureType False [ASTType.i32, ASTType.i32])
  , ("fst_tuple_const", "9", ASTType.i32)
  , ("snd_float", "13.45", ASTType.double)
  , ("snd_bool", "false", ASTType.i1)
  , ("snd_tuple", "Tuple (2, ...)", ASTType.StructureType False [ASTType.i32, ASTType.i32])
  , ("tuple_max", "33", ASTType.i32)


  -- , ("add_sub", 15)
  -- , ("factorial_print", 40320.0) fails bacuase it uses external :(
  -- , ("factorial", 120)
  -- , ("fib_iterative", 0)
  -- , ("fib", 8)
  -- , ("hello_world", 0)
  -- , ("id", 900)
  -- , ("pow_operator", 32)
  -- , ("pow", 64)
  -- , ("sequence_operator", 0)
  -- , ("sub", -1)
  -- , ("unary_minus", -40)
  -- , ("let_in", 12.56)
  -- , ("let_in_multiple", 6)
  ]

-- TODO: compare llvm output
-- TODO: REPL features tests
