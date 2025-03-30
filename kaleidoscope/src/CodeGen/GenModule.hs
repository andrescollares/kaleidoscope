{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module CodeGen.GenModule where

import CodeGen.GenOperand (genOperand)
import CodeGen.LocalVar
  ( LocalVar,
    localVarsFallback,
  )
import CodeGen.Utils.Types (getASTType, operandType, getExpressionType)
import Data.Bifunctor (first)
import Data.Map.Strict (fromList)
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Global (Global (name), basicBlocks, parameters, returnType)
import LLVM.AST.Type (i32, i8, ptr)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder (ParameterName (ParameterName), call, globalStringPtr, extractValue, load, int32, liftModuleState)
import LLVM.IRBuilder.Instruction (ret, store)
import LLVM.IRBuilder.Internal.SnocList (SnocList (SnocList))
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), emitDefn, execModuleBuilder, extern, function, global)
import LLVM.IRBuilder.Monad (IRBuilderT)
import qualified Syntax as S
import LLVM.AST.Attribute (ParameterAttribute(NoAlias))
import LLVM.IRBuilder (gep)
import Debug.Trace
import qualified LLVM.AST.IntegerPredicate
import LLVM.IRBuilder (icmp)
import LLVM.AST.Constant (Constant(Null))
import CodeGen.DataStructures.List (nullIntList)
import LLVM.AST.AddrSpace (AddrSpace(..))
import Control.Monad.RWS (gets)
import Data.List (isPrefixOf)
import Data.String (fromString)
import Data.ByteString.Short (unpack)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Short as SBS

-- Generates the Module from the previous module and the new expressions
-- Has to optimize the module
-- Has to execute the module
-- Has to update the module state
genModule :: [Definition] -> [S.TopLevel] -> [Definition]
genModule oldDefs expressions = buildModuleDefinitions (removeMain oldDefs) newModlState
  where
    removeMain (def : defs) = case def of
      GlobalDefinition AST.Function {name = Name "main"} -> defs
      _ -> def : removeMain defs
    removeMain [] = []
    -- TODO: Could I use the source that's already compiled instead of the previous AST?
    -- use old state and new expressions to generate the new state
    newModlState = mapM genTopLevel expressions

buildModuleDefinitions :: [Definition] -> ModuleBuilder a -> [Definition]
buildModuleDefinitions prevDefs = execModuleBuilder oldModl
  where
    oldModl = ModuleBuilderState {builderDefs = SnocList (reverse prevDefs), builderTypeDefs = typeDefsMap}
    -- TODO: Do we even have type definitions?
    typeDefsMap = fromList $ map (\(TypeDefinition definitionName (Just t)) -> (definitionName, t)) typeDefs
    typeDefs = filter isTypeDef prevDefs
    isTypeDef (TypeDefinition _ _) = True
    isTypeDef _ = False

-- Generates functions, constants, externs, definitions and a main function otherwise
-- The result is a ModuleBuilder monad
genTopLevel :: S.TopLevel -> ModuleBuilder AST.Operand
-- Extern definition
genTopLevel (S.Declaration (S.Extern externName externArgs retType)) = do
  extern externName (map (getASTType . fst) externArgs) (getASTType retType)

-- Function definition
genTopLevel (S.Declaration (S.Function fnName fnArgs retType body)) = do
  let astRetType = getASTType retType
  -- Define the function signature first
  emitDefn $
    GlobalDefinition
      functionDefaults
        { name = fnName,
          parameters = (map (\(t, ParameterName n) -> Parameter (getASTType t) (Name n) []) fnArgs, False),
          returnType = astRetType,
          basicBlocks = []
        }
  -- Generate the function body
  function
    fnName
    (first getASTType <$> fnArgs)
    astRetType
    (genLevel body . localVarsFallback)

-- Constant definition
genTopLevel (S.Declaration (S.Constant (Name constantName) expr)) = do
  case expr of
    S.Float val -> global (Name constantName)  ASTType.double (C.Float (F.Double val))
    S.Int val -> global (Name constantName)  ASTType.i32 (C.Int 32 val)
    S.Bool val -> global (Name constantName)  ASTType.i1 (C.Int 1 (if val then 1 else 0))
    S.TupleI val1 val2 -> do
      let tupleType = ASTType.PointerType (ASTType.StructureType False [getExpressionType val1 [], getExpressionType val2 []]) (AddrSpace 0)
      global (Name constantName)  tupleType (C.Null tupleType)
      function functionName [] i32 $ \_ -> genTupleConstantInitializerFunction expr (BS.unpack (SBS.fromShort constantName)) []
      where
        functionName = Name $ fromString $ "_init_tuple_" ++ BS.unpack (SBS.fromShort constantName)
    _ -> error "Invalid constant definition"

-- Main expression
genTopLevel (S.Expr expression) = do
  function "main" [] i32 (\_ -> genMain expression [])

genTupleConstantInitializerFunction :: S.Expr -> String -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genTupleConstantInitializerFunction e constantName localVars = do
  -- tuple <- gep tupleReference [int32 0, int32 0]
  val <- genOperand e localVars
  store tupleReference 0 val
  ret $ int32 0
  where
    tupleReference = (ConstantOperand (C.GlobalReference (ASTType.PointerType (getExpressionType e []) (AddrSpace 0)) (mkName constantName)))

-- @a = global { i32, { i32, i32 } }* null


-- PointerType {pointerReferent = PointerType {pointerReferent = StructureType {isPacked = False, elementTypes = [StructureType {isPacked = False, elementTypes = [IntegerType {typeBits = 32},IntegerType {typeBits = 32}]}]}, pointerAddrSpace = AddrSpace 0}, pointerAddrSpace = AddrSpace 0}
-- PointerType {pointerReferent = PointerType {pointerReferent = StructureType {isPacked = False, elementTypes = [IntegerType {typeBits = 32},IntegerType {typeBits = 32}]}, pointerAddrSpace = AddrSpace 0}, pointerAddrSpace = AddrSpace 0}

-- define i32 @initTuple() {
--   %1 = alloca { i32, i32 }, align 8
--   %2 = getelementptr { i32, i32 }, { i32, i32 }* %1, i32 0, i32 0
--   store volatile i32 9, i32* %2, align 4
--   %3 = getelementptr { i32, i32 }, { i32, i32 }* %1, i32 0, i32 1
--   store volatile i32 0, i32* %3, align 4
--   %4 = alloca { i32, { i32, i32 }* }, align 8
--   %5 = getelementptr { i32, { i32, i32 }* }, { i32, { i32, i32 }* }* %4, i32 0, i32 0
--   store volatile i32 9, i32* %5, align 4
--   %6 = getelementptr { i32, { i32, i32 }* }, { i32, { i32, i32 }* }* %4, i32 0, i32 1
--   store volatile { i32, i32 }* %1, { i32, i32 }** %6, align 8
--   %7 = alloca { i32, i32 }, align 8
--   %8 = getelementptr { i32, i32 }, { i32, i32 }* %7, i32 0, i32 0
--   store volatile i32 9, i32* %8, align 4
--   %9 = getelementptr { i32, i32 }, { i32, i32 }* %7, i32 0, i32 1
--   store volatile i32 0, i32* %9, align 4
--   %10 = alloca { i32, { i32, i32 }* }, align 8
--   %11 = getelementptr { i32, { i32, i32 }* }, { i32, { i32, i32 }* }* %10, i32 0, i32 0
--   store volatile i32 9, i32* %11, align 4
--   %12 = getelementptr { i32, { i32, i32 }* }, { i32, { i32, i32 }* }* %10, i32 0, i32 1
--   store volatile { i32, i32 }* %7, { i32, i32 }** %12, align 8
--   store { i32, { i32, i32 }* }* %4, { i32, { i32, i32 } }** @a, align 8
--   store { i32, { i32, i32 }* }* %10, { i32, { i32, i32 } }** @a, align 1
--   ret i32 0
-- }

genLevel :: S.Expr -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = do
  operand <- genOperand e localVars
  ret operand

genMain :: S.Expr -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genMain e localVars = do
  currentDefs <- liftModuleState $ gets builderDefs
  let constantDefs = getConstantsFromDefs currentDefs
  case constantDefs of
    Just defs -> generateCallFunctions defs
    Nothing -> return ()
  operand <- genOperand e localVars
  genPrint operand
  ret $ int32 0
  where
  getConstantsFromDefs :: SnocList Definition -> Maybe [Definition]
  getConstantsFromDefs defs = filter initializerPrefixMatch defs
    where
      initializerPrefixMatch :: Definition -> Bool
      initializerPrefixMatch (GlobalDefinition global) = case global of
        AST.Function {name = Name n} -> "_init_tuple_" `isPrefixOf` (BS.unpack (SBS.fromShort n))
        _ -> False 
      initializerPrefixMatch _ = False
      filter :: (a -> Bool) -> SnocList a -> Maybe [a]
      filter _ (SnocList []) = Nothing
      filter f (SnocList (x : xs)) = case filter f (SnocList xs) of
        Just ys -> Just (x : ys)
        Nothing -> if f x then Just [x] else Nothing

  generateCallFunctions :: [Definition] -> IRBuilderT ModuleBuilder ()
  generateCallFunctions defs = mapM_ generateCallFunction defs
    where
      generateCallFunction :: Definition -> IRBuilderT ModuleBuilder ()
      generateCallFunction (GlobalDefinition global) = case global of
        (AST.Function {name = n}) -> call (ConstantOperand (C.GlobalReference (ASTType.ptr (ASTType.FunctionType i32 [] False)) n)) [] >> return ()
        _ -> return ()
      generateCallFunction _ = return ()



genPrint :: AST.Operand -> IRBuilderT ModuleBuilder ()
genPrint operand = mdo
  case operandType operand of
    ASTType.PointerType (ASTType.NamedTypeReference (AST.Name n)) _ -> case n of
      "IntList" -> listPrinterFunction operand "IntList"
      "FloatList" -> listPrinterFunction operand "FloatList"
      "BoolList" -> listPrinterFunction operand "BoolList"
      _ -> error "Unsupported list type for print"
    ASTType.IntegerType {ASTType.typeBits = 1} -> booleanPrinterFunction operand
    _ -> do
      let fmtStr = getFmtStringForType $ operandType operand
      fmtStrGlobal <- globalStringPtr (fmtStr ++ "\n") "fmtStr"

      printArgs <- operandToPrintfArg operand
      _ <- call (ConstantOperand (C.GlobalReference (ASTType.ptr (ASTType.FunctionType i32 [ptr i8] True)) (mkName "printf")))
          ((ConstantOperand fmtStrGlobal, [NoAlias]) : printArgs)
      return ()

-- TODO: move this below to other file

getFmtStringForType :: AST.Type  -> String
getFmtStringForType opType = case opType of
  ASTType.IntegerType {ASTType.typeBits = 32} -> "%d"
  ASTType.FloatingPointType {ASTType.floatingPointType = ASTType.DoubleFP} -> "%f"
  ASTType.IntegerType {ASTType.typeBits = 1} -> "%d"
  ASTType.PointerType {ASTType.pointerReferent = ASTType.StructureType {ASTType.elementTypes = [t1, t2]}} ->
    "(" ++ getFmtStringForType t1 ++ ", " ++ getFmtStringForType t2 ++ ")"
  _ -> error "Unsupported type for printf format"


operandToPrintfArg :: AST.Operand -> IRBuilderT ModuleBuilder [(AST.Operand, [ParameterAttribute])]
operandToPrintfArg operand = case operandType operand of
  ASTType.IntegerType {ASTType.typeBits = 32} -> return [(operand, [])]
  ASTType.FloatingPointType {ASTType.floatingPointType = ASTType.DoubleFP} -> return [(operand, [])]
  -- TODO: Add global variables with "true" and "false" strings to the module and a function that
  --       takes an int1 and returns the corresponding int8* string, that way we can print booleans
  ASTType.IntegerType {ASTType.typeBits = 1} -> return [(operand, [])]
  ASTType.PointerType {ASTType.pointerReferent = ASTType.StructureType {ASTType.elementTypes = [_, _]}} -> do
    tuple <- load operand 0
    val1 <- extractValue tuple [0]
    val2 <- extractValue tuple [1]
    args1 <- operandToPrintfArg val1
    args2 <- operandToPrintfArg val2
    return $ args1 ++ args2
  _ -> error $ "Unsupported type for printf argument" ++ show (operandType operand)

listPrinterFunction :: AST.Operand -> String -> IRBuilderT ModuleBuilder ()
listPrinterFunction operand listPointerName = do
  _ <- call (ConstantOperand (C.GlobalReference (ASTType.ptr (ASTType.FunctionType i32 [PointerType {pointerReferent = NamedTypeReference (mkName listPointerName), pointerAddrSpace = AddrSpace 0}] False)) (mkName $ functionToPrintType listPointerName))) [(operand, [])]
  return ()
  where
    functionToPrintType tp = case tp of
      "IntList" -> "printil"
      "FloatList" -> "printfl"
      "BoolList" -> "printbl"
      _ -> error "Unsupported list type"

booleanPrinterFunction :: AST.Operand -> IRBuilderT ModuleBuilder ()
booleanPrinterFunction operand = do
  _ <- call (ConstantOperand (C.GlobalReference (ASTType.ptr (ASTType.FunctionType ASTType.i1 [ASTType.i1] False)) (mkName "printb"))) [(operand, [])]
  return ()

