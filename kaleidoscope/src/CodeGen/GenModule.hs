{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.GenModule where

import CodeGen.GenOperand (genOperand)
import CodeGen.LocalVar
  ( LocalVar,
    localVarsFallback,
  )
import CodeGen.Utils.Types (syntaxTypeToASTType, operandType)
import Data.Bifunctor (first)
import Data.Map.Strict (fromList)
import LLVM.AST as AST hiding (function)
import LLVM.AST.Attribute (ParameterAttribute)
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global (Global (name), basicBlocks, parameters, returnType)
import LLVM.AST.Type (i32, i8, ptr)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder (ParameterName (ParameterName), call, extractValue, globalStringPtr, int32, load, select)
import LLVM.IRBuilder.Instruction (ret)
import LLVM.IRBuilder.Internal.SnocList (SnocList (SnocList))
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), emitDefn, execModuleBuilder, extern, function)
import LLVM.IRBuilder.Monad (IRBuilderT)
import qualified Syntax as S

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

-- Generates functions, externs, definitions and a main function otherwise
-- The result is a ModuleBuilder monad
genTopLevel :: S.TopLevel -> ModuleBuilder AST.Operand
-- Extern definition
genTopLevel (S.Declaration (S.Extern externName externArgs retType)) = do
  extern externName (map (syntaxTypeToASTType . fst) externArgs) (syntaxTypeToASTType retType)

-- Function definition
genTopLevel (S.Declaration (S.Function fnName fnArgs retType body)) = do
  let astRetType = syntaxTypeToASTType retType
  -- Define the function signature first
  emitDefn $
    GlobalDefinition
      functionDefaults
        { name = fnName,
          parameters = (map (\(t, ParameterName n) -> Parameter (syntaxTypeToASTType t) (Name n) []) fnArgs, False),
          returnType = astRetType,
          basicBlocks = []
        }
  -- Generate the function body
  function
    fnName
    (first syntaxTypeToASTType <$> fnArgs)
    astRetType
    (genLevel body . localVarsFallback)

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
  let fmtStr = getFmtStringForType $ operandType operand
  fmtStrGlobal <- globalStringPtr (fmtStr ++ "\n") "fmtStr"

  printArgs <- operandToPrintfArg operand
  _ <-
    call
      (ConstantOperand (C.GlobalReference (ASTType.ptr (ASTType.FunctionType i32 [ptr i8] True)) (mkName "printf")))
      ((ConstantOperand fmtStrGlobal, []) : printArgs)
  return ()

getFmtStringForType :: AST.Type -> String
getFmtStringForType opType = case opType of
  ASTType.IntegerType {ASTType.typeBits = 32} -> "%d"
  ASTType.FloatingPointType {ASTType.floatingPointType = ASTType.DoubleFP} -> "%f"
  -- HACK: just "%s" results in an error ¯\_(ツ)_/¯
  ASTType.IntegerType {ASTType.typeBits = 1} -> "%se"
  ASTType.PointerType {ASTType.pointerReferent = ASTType.StructureType {ASTType.elementTypes = [t1, t2]}} ->
    "(" ++ getFmtStringForType t1 ++ ", " ++ getFmtStringForType t2 ++ ")"
  ASTType.PointerType {ASTType.pointerReferent = ASTType.PointerType {ASTType.pointerReferent = ASTType.VoidType}} -> ""
  _ -> error "Unsupported type for printf format: " ++ show opType

operandToPrintfArg :: AST.Operand -> IRBuilderT ModuleBuilder [(AST.Operand, [ParameterAttribute])]
operandToPrintfArg operand = case operandType operand of
  ASTType.IntegerType {ASTType.typeBits = 32} -> return [(operand, [])]
  ASTType.FloatingPointType {ASTType.floatingPointType = ASTType.DoubleFP} -> return [(operand, [])]
  ASTType.IntegerType {ASTType.typeBits = 1} -> do
    -- Convert boolean to string pointer directly using select
    ts <- globalStringPtr "tru" "t_str"
    fs <- globalStringPtr "fals" "f_str"

    strPtr <- select operand (ConstantOperand ts) (ConstantOperand fs)
    return [(strPtr, [])]
  ASTType.PointerType {ASTType.pointerReferent = ASTType.StructureType {ASTType.elementTypes = [_, ASTType.VoidType]}} -> do
    tuple <- load operand 0
    val1 <- extractValue tuple [0]
    operandToPrintfArg val1
  ASTType.PointerType {ASTType.pointerReferent = ASTType.StructureType {ASTType.elementTypes = [_, _]}} -> do
    tuple <- load operand 0
    val1 <- extractValue tuple [0]
    val2 <- extractValue tuple [1]
    args1 <- operandToPrintfArg val1
    args2 <- operandToPrintfArg val2
    return $ args1 ++ args2
  ASTType.PointerType {ASTType.pointerReferent = ASTType.PointerType {ASTType.pointerReferent = ASTType.VoidType}} -> do
    return []
  _ -> error $ "Unsupported type for printf argument: " ++ show (operandType operand)