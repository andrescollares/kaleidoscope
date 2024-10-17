{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module CodeGen.GenModule where

import Control.Monad.RWS (gets)
import Data.Bifunctor (first)
import CodeGen.GenOperand (genOperand)
import CodeGen.LocalVar
  ( LocalVar,
    definitionsToLocalVars,
    functionLocalVar,
  )
import CodeGen.JIT (optimizeModule, runJIT)
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Global (Global (name))
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Instruction (ret)
import LLVM.IRBuilder.Internal.SnocList (SnocList (SnocList))
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), MonadModuleBuilder (liftModuleState), execModuleBuilder, extern, function, global, typedef)
import LLVM.IRBuilder.Monad (IRBuilderT)
import qualified Syntax as S
import CodeGen.Utils.Types ( getExpressionType, getASTType )
import CLIParameters (CLIParameters)
import Data.Map.Strict (Map, fromList)
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import Data.String (fromString)

-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Definition] -> [S.Expr] -> CLIParameters -> IO (String, [Definition])
genModule oldDefs expressions options = do
  -- mapM_ print expressions
  optMod <-  optimizeModule unoptimizedAst options
  res <- runJIT optMod
  return (res, definitions)
  where
    -- TODO: Remove old duplicate functions
    -- use old state and new expressions to generate the new state
    modlState = mapM genTopLevel expressions
    oldDefsWithoutMain =
      filter -- TODO: filter first
        ( \case
            GlobalDefinition AST.Function {name = Name "main"} -> False
            _ -> True
        )
        oldDefs
    definitions = buildModuleWithDefinitions oldDefsWithoutMain modlState
    unoptimizedAst = mkModule definitions
    mkModule ds = defaultModule {moduleName = "kaleidoscope", moduleDefinitions = ds}

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
genTopLevel :: S.Expr -> ModuleBuilder AST.Operand
-- Extern definition
genTopLevel (S.TopLevel (S.Extern externName externArgs S.Integer)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.i32
genTopLevel (S.TopLevel (S.Extern externName externArgs S.Double)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.double
genTopLevel (S.TopLevel (S.Extern externName externArgs S.Boolean)) = do
  extern externName (map (getASTType . fst) externArgs) ASTType.i1

-- Function definition
genTopLevel (S.TopLevel (S.Function functionName functionArgs S.Double body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.double
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.double
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs S.Integer body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.i32
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.i32
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs S.Boolean body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.i1
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.i1
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs (S.Tuple t1 t2) body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.StructureType {AST.isPacked = False, AST.elementTypes = [getASTType t1, getASTType t2]}
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName ASTType.StructureType {AST.isPacked = False, AST.elementTypes = [getASTType t1, getASTType t2]}
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs (S.ListType S.Integer) body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (AST.Name $ fromString "IntList"), pointerAddrSpace = AddrSpace 0 }
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName (ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (AST.Name $ fromString "IntList"), pointerAddrSpace = AddrSpace 0 })
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs (S.ListType S.Double) body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (AST.Name $ fromString "FloatList"), pointerAddrSpace = AddrSpace 0 }
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName (ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (AST.Name $ fromString "IntList"), pointerAddrSpace = AddrSpace 0 })
    )
genTopLevel (S.TopLevel (S.Function functionName functionArgs (S.ListType S.Boolean) body)) = do
  function
    functionName
    (first getASTType <$> functionArgs)
    ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (AST.Name $ fromString "BoolList"), pointerAddrSpace = AddrSpace 0 }
    ( \ops ->
        genLevel body $ functionLocalVar ops functionArgs functionName (ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (AST.Name $ fromString "IntList"), pointerAddrSpace = AddrSpace 0 })
    )

-- Constant definition
genTopLevel (S.TopLevel (S.Constant S.Double constantName (S.Float val))) = do
  global constantName ASTType.double (C.Float (F.Double val))
genTopLevel (S.TopLevel (S.Constant S.Integer constantName (S.Int val))) = do
  global constantName ASTType.i32 (C.Int 32 val)
genTopLevel (S.TopLevel (S.Constant S.Boolean constantName (S.Bool val))) = do
  global constantName ASTType.i1 (C.Int 1 (if val then 1 else 0))
genTopLevel (S.TopLevel (S.Constant (S.Tuple t1 t2) constantName (S.TupleI e1 e2))) = do
  global constantName (ASTType.StructureType False [getASTType t1, getASTType t2])
   (C.Struct {C.structName = Nothing, C.isPacked = False, C.memberValues = [
      constantOperand e1,
      constantOperand e2
   ]})
  where
    constantOperand (S.Float n) = C.Float (F.Double n)
    constantOperand (S.Int n) = C.Int 32 n
    constantOperand (S.Bool b) = C.Int 1 (if b then 1 else 0)
    constantOperand (S.TupleI _ _) = error "TODO: recursive tuple constant"
genTopLevel (S.TopLevel (S.Constant (S.ListType S.Integer) constantName (S.List list))) = error "TODO: list constant"
  -- List constants are not supported yet
  -- LLVM-hs can't create a pointer constant
  -- see: https://groups.google.com/g/llvm-dev/c/KGnZRoyaD48/m/XREo3ystxCAJ
  -- global constantName listType
  --   (C.Null listType)
  -- where listType = ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (AST.Name $ fromString "IntList"), pointerAddrSpace = AddrSpace 0 }

genTopLevel (S.TopLevel S.Constant {}) = error "Invalid constant definition"

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
    printerWrapper exprs currentDefs = S.Call (printerFunctionName (eType currentDefs)) (exprs : printerExtraParams (eType currentDefs))
    printerFunctionName (FloatingPointType _) = "printd"
    printerFunctionName IntegerType { ASTType.typeBits = 32 } = "printi"
    printerFunctionName IntegerType { ASTType.typeBits = 1 } = "printb"
    printerFunctionName (PointerType (NamedTypeReference (Name "IntList")) (AddrSpace 0)) = "printil"
    printerFunctionName (PointerType (NamedTypeReference (Name "FloatList")) (AddrSpace 0)) = "printfl"
    printerFunctionName (PointerType (NamedTypeReference (Name "BoolList")) (AddrSpace 0)) = "printbl"
    printerFunctionName StructureType { ASTType.elementTypes = _ } = "print_tuple"
    printerFunctionName _ = error "Unsupported type for print function"
    printerExtraParams StructureType { ASTType.elementTypes = [t1, t2] } = case (t1, t2) of
      (IntegerType { ASTType.typeBits = 32 }, IntegerType { ASTType.typeBits = 32 }) -> [S.Int 1, S.Int 1]
      (IntegerType { ASTType.typeBits = 32 }, FloatingPointType { ASTType.floatingPointType = DoubleFP }) -> [S.Int 1, S.Int 2]
      (IntegerType { ASTType.typeBits = 32 }, IntegerType { ASTType.typeBits = 1 }) -> [S.Int 1, S.Int 3]
      (FloatingPointType { ASTType.floatingPointType = DoubleFP }, IntegerType { ASTType.typeBits = 32 }) -> [S.Int 2, S.Int 1]
      (FloatingPointType { ASTType.floatingPointType = DoubleFP }, FloatingPointType { ASTType.floatingPointType = DoubleFP }) -> [S.Int 2, S.Int 2]
      (FloatingPointType { ASTType.floatingPointType = DoubleFP }, IntegerType { ASTType.typeBits = 1 }) -> [S.Int 2, S.Int 3]
      (IntegerType { ASTType.typeBits = 1 }, IntegerType { ASTType.typeBits = 32 }) -> [S.Int 3, S.Int 1]
      (IntegerType { ASTType.typeBits = 1 }, FloatingPointType { ASTType.floatingPointType = DoubleFP }) -> [S.Int 3, S.Int 2]
      (IntegerType { ASTType.typeBits = 1 }, IntegerType { ASTType.typeBits = 1 }) -> [S.Int 3, S.Int 3]
      _ -> []
    printerExtraParams _ = []

-- -- Type definition: this is a no-op
genTopLevel (S.TopLevel (S.TypeDef typeName typeDef)) = do
  _ <- typedef typeName (Just $ getASTType typeDef)
  global "dummy" ASTType.i32 (C.Int 32 0)
  -- TODO: do nothing (no-op)

genTopLevel _ = error "This shouldn't have matched here."

genLevel :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = genOperand e localVars >>= ret