{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder where

import Data.ByteString.Short
import JIT
import LLVM.AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.IntegerPredicate as P
import LLVM.AST.Type as AST
import LLVM.IRBuilder.Constant as Con
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad

data Expr
  = SFloat Double
  | SFunction Name [ParameterName] Expr
  deriving stock (Eq, Ord, Show)

simple :: Module
simple = buildModule "exampleModule" $ do
  function "plus" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    r <- add x y
    ret r

conditional :: Module
conditional = buildModule "conditionModule" $ do
  -- Breaks SSA Principle: https://stackoverflow.com/a/70901888
  -- f <- function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
  --   cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
  --   condBr cond ifThen ifElse
  --   ifThen <- block `named` "if.then"
  --   trVal <- add a (ConstantOperand (C.Int 32 0))
  --   ret trVal
  --   -- br ifExit
  --   ifElse <- block `named` "if.else"
  --   flVal <- add a (ConstantOperand (C.Int 32 0))
  --   ret flVal

  f <- function "f" [(AST.i32, "a")] AST.i32 $ \[a] -> mdo
    cond <- icmp P.EQ a (ConstantOperand (C.Int 32 0))
    condBr cond ifThen ifElse
    ifThen <- block `named` "if.then"
    trVal <- add a (ConstantOperand (C.Int 32 0))
    ret trVal
    br ifExit
    ifElse <- block `named` "if.else"
    flVal <- add a (ConstantOperand (C.Int 32 0))
    ret flVal
    br ifExit
    ifExit <- block `named` "if.exit"
    -- SSA
    r <- phi [(trVal, ifThen), (flVal, ifElse)]
    ret r

  function "main" [] AST.i32 $ \[] -> mdo
    -- the empty array are the parameter attributes: https://hackage.haskell.org/package/llvm-hs-pure-9.0.0/docs/LLVM-AST-ParameterAttribute.html
    r <- call f [(ConstantOperand (C.Int 32 0), [])]
    ret r

arithmetrics :: Module
arithmetrics = buildModule "arithmetrics" $ do
  function "+" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    r <- add x y
    ret r

  function "-" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    r <- sub x y
    ret r

  function "*" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    r <- mul x y
    ret r

  function "/" [(AST.i32, "x"), (AST.i32, "y")] AST.i32 $ \[x, y] -> do
    r <- sdiv x y
    ret r

globalDef :: Module
globalDef = buildModule "variable_test" $ do
  x <- global "x" AST.double (C.Float (F.Double 50.0))

  function "main" [] AST.double $ \[] -> do
    x1 <- load x 0
    r <- fadd x1 (ConstantOperand (C.Float (F.Double 1.0)))
    -- res <- call (ConstantOperand (C.GlobalReference (ptr (FunctionType AST.i32 [ ] False)) (Name "f")) ) [ ]
    ret r

genOptimizedMainModuleIR :: IO Module
genOptimizedMainModuleIR = genModule [SFloat 5.0]

-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Expr] -> IO Module
genModule expressions = do
  res <- optimizeModule unoptimizedAst
  runJIT res
  return res
  where
    -- use old state and new expressions to generate the new state
    modlState = mapM genTopLevel expressions
    unoptimizedAst = buildModule "kaleidoscope" modlState

-- Generates functions, constants, externs, definitions and a main function otherwise
-- The result is a ModuleBuilder monad
genTopLevel :: Expr -> ModuleBuilder Operand
genTopLevel (SFunction name args body) = do
  function name (map (\x -> (AST.double, x)) args) AST.double (genOperand body args)
genTopLevel expression = do
  function "main" [] AST.double (genOperand expression [])

-- Generates the Operands that codegenTop needs.
genOperand :: Expr -> [ParameterName] -> ([Operand] -> IRBuilderT ModuleBuilder ())
genOperand (SFloat n) [] = \_ -> ret $ ConstantOperand (C.Float (F.Double n))
