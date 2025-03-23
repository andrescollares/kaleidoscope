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
import CodeGen.Utils.Types (getASTType, operandType)
import Data.Bifunctor (first)
import Data.Map.Strict (fromList)
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.Global (Global (name), basicBlocks, parameters, returnType)
import LLVM.AST.Type (i32, i8, ptr)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder (ParameterName (ParameterName), call, globalStringPtr, extractValue, load, int32)
import LLVM.IRBuilder.Instruction (ret)
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
genTopLevel (S.Declaration (S.Constant constantName expr)) = do
  case expr of
    S.Float val -> global constantName ASTType.double (C.Float (F.Double val))
    S.Int val -> global constantName ASTType.i32 (C.Int 32 val)
    S.Bool val -> global constantName ASTType.i1 (C.Int 1 (if val then 1 else 0))
    _ -> error "Invalid constant definition"

-- Main expression
genTopLevel (S.Expr expression) = do
  function "main" [] i32 (\_ -> genMain expression [])

genLevel :: S.Expr -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genLevel e localVars = do
  operand <- genOperand e localVars
  ret operand

genMain :: S.Expr -> [LocalVar] -> IRBuilderT ModuleBuilder ()
genMain e localVars = do
  operand <- genOperand e localVars
  genPrint operand
  ret $ int32 0

genPrint :: AST.Operand -> IRBuilderT ModuleBuilder ()
genPrint operand = mdo
  case operandType operand of
    ASTType.PointerType (ASTType.NamedTypeReference (AST.Name n)) _ -> case n of
      "IntList" -> listPrinterFunction operand "IntList"
      "FloatList" -> listPrinterFunction operand "FloatList"
      "BoolList" -> listPrinterFunction operand "BoolList"
      _ -> error "Unsupported list type for print"
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
  _ -> error "Unsupported type for printf argument"

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
    
