{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder where

import Data.ByteString.Short

import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import LLVM.AST.Float
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.AST.Type as AST

import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Constant as Con

simple :: Module
simple = buildModule "exampleModule" $ do
  function "plus" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    _entry <- block `named` "entry2"
    r <- add x y
    ret r

condition :: Module
condition = buildModule "conditionModule" $ do
  function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
    _entry <- block `named` "entry"
    cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
    condBr cond ifThen ifElse
    ifThen <- block
    trVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifElse <- block `named` "if.else"
    flVal <- add a (ConstantOperand (C.Int 32 0))
    br ifExit
    ifExit <- block `named` "if.exit"
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r

arithmetrics :: Module
arithmetrics = buildModule "arithmetrics" $ do
  function "+" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    entry <- block `named` "entry"
    r <- add x y
    ret r

  function "-" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    entry <- block `named` "entry2"
    r <- sub x y
    ret r

  function "*" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    entry <- block `named` "entry3"
    r <- mul x y
    ret r

  function "/" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    entry <- block `named` "entry4"
    r <- sdiv x y
    ret r

globalDef :: Module
globalDef = buildModule "variable_test" $ do
  x <- global "x" AST.double (C.Float (Double 50.0))

  function "main" [] AST.double $ \[] -> do
    entry <- block `named` "entry"
    x1 <- load x 0
    r <- fadd x1 (ConstantOperand (C.Float (Double 1.0)))
    -- res <- call (ConstantOperand (C.GlobalReference (ptr (FunctionType AST.i32 [] False)) (Name "f"))) []
    ret r

-- Next objective, implement a recursive way to generate the AST Module
