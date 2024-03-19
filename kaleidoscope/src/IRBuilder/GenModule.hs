{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder.GenModule where

-- import Data.Maybe

import Control.Monad.RWS (gets)
import Data.Bifunctor (first)
import Data.ByteString.Short
import qualified Data.List as DL
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.Text as T
import IRBuilder.GenOperand (genOperand)
import IRBuilder.LocalVar
import Instructions
import JIT
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (UEQ, UGE, UGT, ULE, ULT, UNE))
import LLVM.AST.Global (Global (GlobalVariable, name, type'), parameters, returnType)
import qualified LLVM.AST.Global as G (Global (name, type'))
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Type (ptr)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Internal.SnocList
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), MonadModuleBuilder (liftModuleState), ParameterName (ParameterName), execModuleBuilder, extern, function, global)
import LLVM.IRBuilder.Monad
import Syntax as S
import Types

-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Definition] -> [Expr] -> IO (Double, [Definition])
genModule oldDefs expressions = do
  optMod <- optimizeModule unoptimizedAst
  res <- runJIT optMod moduleMainFnType
  return (res, definitions)
  where
    -- TODO: Remove old duplicate functions
    -- use old state and new expressions to generate the new state
    modlState = mapM genTopLevel expressions
    oldDefsWithoutMain =
      filterFst
        ( \case
            GlobalDefinition AST.Function {name = Name "main"} -> True
            _ -> False
        )
        oldDefs
    filterFst _ [] = []
    filterFst p (x : xs)
      | p x = xs
      | otherwise = x : filterFst p xs

    definitions = buildModuleWithDefinitions oldDefsWithoutMain modlState
    unoptimizedAst = mkModule definitions
    mkModule ds = defaultModule {moduleName = "kaleidoscope", moduleDefinitions = ds}
    moduleMainFn =
      filter
        ( \case
            GlobalDefinition AST.Function {name = Name "main"} -> True
            _ -> False
        )
        definitions
    moduleMainFnType = case moduleMainFn of
      [GlobalDefinition AST.Function {returnType = IntegerType {typeBits = 32}}] -> ASTType.i32
      _ -> ASTType.double

buildModuleWithDefinitions :: [Definition] -> ModuleBuilder a -> [Definition]
buildModuleWithDefinitions prevDefs = execModuleBuilder oldModl
  where
    oldModl = ModuleBuilderState {builderDefs = SnocList (reverse prevDefs), builderTypeDefs = mempty}

-- Generates functions, constants, externs, definitions and a main function otherwise
-- The result is a ModuleBuilder monad
genTopLevel :: Expr -> ModuleBuilder AST.Operand
-- Extern definition
genTopLevel (S.TopLevel (S.Extern externName externArgs Integer)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.i32
genTopLevel (S.TopLevel (S.Extern externName externArgs Double)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.double
genTopLevel (S.TopLevel (S.Extern externName externArgs Boolean)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.i1
-- Function definition
genTopLevel (S.TopLevel (S.Function functionName functionArgs Double body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.double
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.double
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs Integer body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.i32
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.i32
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs Boolean body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.i1
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.i1
    )
-- Constant definition
genTopLevel (S.TopLevel (S.Constant Double constantName (Float val))) = do
  global constantName ASTType.double (C.Float (F.Double val))
genTopLevel (S.TopLevel (S.Constant Integer constantName (Int val))) = do
  global constantName ASTType.i32 (C.Int 32 val)
genTopLevel (S.TopLevel (S.Constant Boolean constantName (Bool val))) = do
  global constantName ASTType.i1 (C.Int 1 (if val then 1 else 0))
genTopLevel (S.TopLevel (S.Constant {})) = error "This shouldn't have matched here."
-- Unary operator definition
genTopLevel (S.Operand (S.UnaryDef unaryOpName unaryArgs body)) = do
  function (Name ("unary_" <> unaryOpName)) (map (\x -> (ASTType.double, x)) unaryArgs) ASTType.double (\_ -> genLevel body []) -- TODO: localVars
  -- Binary operator definition
genTopLevel (S.Operand (S.BinaryDef binaryOpName binaryArgs body)) = do
  function (Name ("binary_" <> binaryOpName)) (map (\x -> (ASTType.double, x)) binaryArgs) ASTType.double (\_ -> genLevel body []) -- TODO: localVars
  -- Any expression
genTopLevel (S.Operand expression) = do
  currentDefs <- liftModuleState $ gets builderDefs
  function "main" [] (eType currentDefs) (\_ -> genLevel expression [])
  where
    -- Determine type of expression to be used as return type of main function
    eType currentDefs = getExpressionType expression $ definitionsToLocalVars currentDefs

genLevel :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = do
  generated <- genOperand e localVars
  ret generated

-- Local vars