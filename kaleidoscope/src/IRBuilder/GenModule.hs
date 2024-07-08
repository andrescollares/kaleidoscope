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
import Types ( getExpressionType, getASTType, findTypeAlias )
import CLI (CliOptions)
import Data.Map.Strict (Map, fromList)
import Debug.Trace
import Data.Foldable (find)
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))


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
      filter
        ( \case
            GlobalDefinition AST.Function {name = Name "main"} -> False
            GlobalDefinition AST.Function {name = Name "printResult"} -> False
            _ -> True
        )
        oldDefs
    definitions = buildModuleWithDefinitions oldDefsWithoutMain modlState
    unoptimizedAst = mkModule definitions
    mkModule ds = defaultModule {moduleName = "kaleidoscope", moduleDefinitions = ds}
    moduleMainFn =
      find
        ( \case
            GlobalDefinition AST.Function {name = Name "main"} -> True
            _ -> False
        )
        definitions
    moduleMainFnType = case moduleMainFn of
      Just (GlobalDefinition AST.Function {returnType = IntegerType {typeBits = 32}}) -> ASTType.i32
      Just (GlobalDefinition AST.Function {returnType = IntegerType {typeBits = 1}}) -> ASTType.i1
      Just (GlobalDefinition AST.Function {returnType = FloatingPointType {floatingPointType = DoubleFP}}) -> ASTType.double
      Just (GlobalDefinition AST.Function {returnType = StructureType { elementTypes = [t1, t2] }}) -> ASTType.StructureType { elementTypes = [t1, t2], isPacked = False}
      Just (GlobalDefinition AST.Function {returnType = NamedTypeReference (Name "IntList")}) -> NamedTypeReference (Name "IntList")
      Nothing -> error "Main function not found"

type TypeDefinitionMap = Map Name AST.Type

buildModuleWithDefinitions :: [Definition] -> ModuleBuilder a -> [Definition]
buildModuleWithDefinitions prevDefs = execModuleBuilder oldModl
  where
    oldModl = ModuleBuilderState {builderDefs = SnocList (reverse prevDefs), builderTypeDefs = defsMap}
    defsMap = fromList $ map (\(TypeDefinition definitionName (Just t)) -> (definitionName, t)) typeDefs
    typeDefs = filter isTypeDef prevDefs
    isTypeDef (TypeDefinition _ _) = True
    isTypeDef _ = False


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
  currentDefs <- liftModuleState $ gets builderDefs
  function "main" [] (eType currentDefs) (\_ -> genLevel (printerWrapper expression currentDefs) [])
  -- function "printResult" [] (eType currentDefs) (\_ -> genLevel (printerExpressions currentDefs) [])
  where
    -- Determine type of expression to be used as return type of main function
    eType currentDefs = getExpressionType expression $ definitionsToLocalVars currentDefs
    -- typeDefs moduleDefs = findTypeAlias (Name "a") moduleDefs
    -- TODO: findTypeAlias can get a AST.Type to use when we use a name type alias
    printerWrapper exprs currentDefs = S.Call (printerFunctionName (eType currentDefs)) [exprs]
    printerFunctionName (FloatingPointType _) = "printd"
    printerFunctionName (IntegerType { ASTType.typeBits = 32 }) = "printi"
    printerFunctionName (IntegerType { ASTType.typeBits = 1 }) = "printb"
    printerFunctionName (PointerType (NamedTypeReference (Name "IntList")) (AddrSpace 0)) = "printil"
    printerFunctionName (PointerType (NamedTypeReference (Name "FloatList")) (AddrSpace 0)) = "printfl"
    printerFunctionName (PointerType (NamedTypeReference (Name "BoolList")) (AddrSpace 0)) = "printbl"
    printerFunctionName (StructureType { ASTType.elementTypes = [t1, t2] }) = "TODO: print tuple"
    printerFunctionName _ = error "Unsupported type for print function"

-- -- Type definition: this is a no-op
genTopLevel (S.TopLevel (S.TypeDef typeName typeDef)) = do
  _ <- typedef typeName (Just $ getASTType typeDef)
  global "dummy" ASTType.i32 (C.Int 32 0)
  -- TODO: do nothing (no-op)

genTopLevel _ = error "This shouldn't have matched here."

-- TODO: add print statement to show result value
genLevel :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = do
  generated <- genOperand e localVars
  ret generated

