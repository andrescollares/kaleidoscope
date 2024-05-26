{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module IRBuilder.GenModule where

-- import Data.Maybe

import Control.Monad.RWS (gets)
import Data.Bifunctor (first)
import IRBuilder.GenOperand (genOperand)
import IRBuilder.LocalVar
  ( LocalVar,
    definitionsToLocalVars,
    functionLocalVar,
  )
import JIT (optimizeModule, runJIT)
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Global (Global (name), returnType)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Instruction (ret)
import LLVM.IRBuilder.Internal.SnocList (SnocList (SnocList))
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), MonadModuleBuilder (liftModuleState), execModuleBuilder, extern, function, global, typedef)
import LLVM.IRBuilder.Monad (IRBuilderT)
import Syntax as S
import Types ( getExpressionType, getASTType )
import CLI (CliOptions)
import Data.Map.Strict (Map, fromList)

import Data.Map.Strict (size)
import Debug.Trace

-- All defs: [GlobalDefinition (Function {linkage = External, visibility = Default, dllStorageClass = Nothing, callingConvention = C, returnAttributes = [], returnType = IntegerType {typeBits = 32}, name = Name "printi", parameters = ([Parameter (IntegerType {typeBits = 32}) (Name "i") []],False), functionAttributes = [], section = Nothing, comdat = Nothing, alignment = 0, garbageCollectorName = Nothing, prefix = Nothing, basicBlocks = [], personalityFunction = Nothing, metadata = []}),GlobalDefinition (Function {linkage = External, visibility = Default, dllStorageClass = Nothing, callingConvention = C, returnAttributes = [], returnType = FloatingPointType {floatingPointType = DoubleFP}, name = Name "printd", parameters = ([Parameter (FloatingPointType {floatingPointType = DoubleFP}) (Name "d") []],False), functionAttributes = [], section = Nothing, comdat = Nothing, alignment = 0, garbageCollectorName = Nothing, prefix = Nothing, basicBlocks = [], personalityFunction = Nothing, metadata = []}),GlobalDefinition (Function {linkage = External, visibility = Default, dllStorageClass = Nothing, callingConvention = C, returnAttributes = [], returnType = IntegerType {typeBits = 1}, name = Name "printb", parameters = ([Parameter (IntegerType {typeBits = 1}) (Name "b") []],False), functionAttributes = [], section = Nothing, comdat = Nothing, alignment = 0, garbageCollectorName = Nothing, prefix = Nothing, basicBlocks = [], personalityFunction = Nothing, metadata = []}),GlobalDefinition (GlobalVariable {name = Name "dummy", linkage = External, visibility = Default, dllStorageClass = Nothing, threadLocalMode = Nothing, unnamedAddr = Nothing, isConstant = False, type' = IntegerType {typeBits = 32}, addrSpace = AddrSpace 0, initializer = Just (Int {integerBits = 32, integerValue = 0}), section = Nothing, comdat = Nothing, alignment = 0, metadata = []})]


-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Definition] -> [Expr] -> CliOptions -> IO (String, [Definition])
genModule oldDefs expressions options = do
  optMod <- optimizeModule unoptimizedAst options
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
    oldTypeDefs = filter
      ( \case
          TypeDefinition {} -> True
          _ -> False
      )
      oldDefs
    filterFst _ [] = []
    filterFst p (x : xs)
      | p x = xs
      | otherwise = x : filterFst p xs
    modlTypesState = mapM genTypes expressions
    newModlTypes = buildModuleWithTypeDefinitions oldTypeDefs modlTypesState
    definitions = newModlTypes ++ buildModuleWithDefinitions oldDefsWithoutMain newModlTypes modlState
    unoptimizedAst = trace ("Module defs: " ++ show definitions) $ mkModule definitions
    mkModule ds = defaultModule {moduleName = "kaleidoscope", moduleDefinitions = ds}
    moduleMainFn =
      filter -- Filter fst TODO:
        ( \case
            GlobalDefinition AST.Function {name = Name "main"} -> True
            _ -> False
        )
        definitions
    moduleMainFnType = case moduleMainFn of
      [GlobalDefinition AST.Function {returnType = IntegerType {typeBits = 32}}] -> ASTType.i32
      [GlobalDefinition AST.Function {returnType = FloatingPointType {floatingPointType = DoubleFP}}] -> ASTType.double
      [GlobalDefinition AST.Function {returnType = StructureType { elementTypes = [t1, t2] }}] -> ASTType.StructureType { elementTypes = [t1, t2] }
      _ -> ASTType.i1

type TypeDefinitionMap = Map Name AST.Type

buildModuleWithTypeDefinitions :: [Definition] -> ModuleBuilder a -> [Definition]
buildModuleWithTypeDefinitions prevDefs = execModuleBuilder oldModl
  where
    oldModl = ModuleBuilderState {builderDefs = SnocList [], builderTypeDefs = defsMap}
    defsMap = fromList $ map (\(TypeDefinition definitionName (Just t)) -> (definitionName, t)) prevDefs

buildModuleWithDefinitions :: [Definition] -> [Definition] -> ModuleBuilder a -> [Definition]
buildModuleWithDefinitions prevDefs typeDefs = execModuleBuilder oldModl
  where
    oldModl = ModuleBuilderState {builderDefs = SnocList (reverse prevDefs), builderTypeDefs = defsMap}
    defsMap = fromList $ map (\(TypeDefinition definitionName (Just t)) -> (definitionName, t)) typeDefs


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
genTopLevel (S.TopLevel (S.Function functionName functionArgs (Tuple t1 t2) body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.StructureType {AST.isPacked = False, AST.elementTypes = [getASTType t1, getASTType t2]}
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.StructureType {AST.isPacked = False, AST.elementTypes = [getASTType t1, getASTType t2]}
    )

-- Constant definition
genTopLevel (S.TopLevel (S.Constant Double constantName (Float val))) = do
  global constantName ASTType.double (C.Float (F.Double val))
genTopLevel (S.TopLevel (S.Constant Integer constantName (Int val))) = do
  global constantName ASTType.i32 (C.Int 32 val)
genTopLevel (S.TopLevel (S.Constant Boolean constantName (Bool val))) = do
  global constantName ASTType.i1 (C.Int 1 (if val then 1 else 0))
genTopLevel (S.TopLevel (S.Constant (Tuple t1 t2) constantName (TupleI e1 e2))) = do
  global constantName (ASTType.StructureType False [getASTType t1, getASTType t2])
   (C.Struct {C.structName = Nothing, C.isPacked = False, C.memberValues = [
      constantOperand e1,
      constantOperand e2
   ]})
  where
    constantOperand (Float n) = C.Float (F.Double n)
    constantOperand (Int n) = C.Int 32 n
    constantOperand (Bool b) = C.Int 1 (if b then 1 else 0)
    constantOperand (TupleI a b) = error "TODO: recursive tuple constant"
genTopLevel (S.TopLevel (S.Constant {})) = error "Invalid constant definition"

-- Main expression
genTopLevel (S.Operand expression) = do
  typeDefs <- liftModuleState $ gets builderTypeDefs
  currentDefs <- trace ("Type defs:" ++ show typeDefs) $ liftModuleState $ gets builderDefs
  function "main" [] (eType currentDefs) (\_ -> genLevel expression [])
  where
    -- Determine type of expression to be used as return type of main function
    eType currentDefs = getExpressionType expression $ definitionsToLocalVars currentDefs

-- -- Type definition: this is a no-op
genTopLevel (S.TopLevel (S.TypeDef _ _)) = do
  global "dummy" ASTType.i32 (C.Int 32 0)
  -- TODO: do nothing (no-op)

genTopLevel _ = error "This shouldn't have matched here."

genLevel :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = do
  generated <- genOperand e localVars
  ret generated

genTypes :: Expr -> ModuleBuilder AST.Type
genTypes (S.TopLevel (S.TypeDef typeName typeDef)) = do
  typedef typeName (Just $ getASTType typeDef)

-- do a no-op
genTypes _ = return ASTType.i32

