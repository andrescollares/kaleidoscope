{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder.GenOperand where

import Control.Monad.RWS (gets)
import Data.ByteString.Short ( ShortByteString )
import qualified Data.Map.Strict as M
import IRBuilder.LocalVar
  ( LocalVar,
    getConstantFromDefs,
    getFunctionFromDefs,
    getFunctionOperand,
    getLocalVarName
  )
import Instructions (typedOperandInstruction, operandType)
import LLVM.AST as AST
  ( Definition (GlobalDefinition),
    Global (Function, GlobalVariable),
    Name (Name),
    Operand (ConstantOperand),
  )
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (UEQ, UGE, UGT, ULE, ULT, UNE))
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder (ModuleBuilder, builderDefs, liftModuleState)
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Monad (IRBuilderT, block, named)
import Syntax as S
import Types (getASTType, getExpressionType, listPointerTypeName)
import Tuple (tupleAccessorOperand)
import List (nullIntList)
import LLVM.IRBuilder.Internal.SnocList (SnocList)
import qualified LLVM.AST.AddrSpace as AST
import Data.String (fromString)


-- Generates the Operands that genTopLevel needs.
genOperand :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder AST.Operand
-- Float
genOperand (Float n) _ = return $ ConstantOperand (C.Float (F.Double n))
-- Integer
genOperand (Int n) _ = return $ ConstantOperand (C.Int 32 n)
-- Bool
genOperand (Bool b) _ = return $ ConstantOperand (C.Int 1 (if b then 1 else 0))
-- Tuple
genOperand (TupleI a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  return $ ConstantOperand (C.Struct {C.structName = Nothing, C.isPacked = False, C.memberValues = [getConstant opA, getConstant opB]})
  where
    getConstant (ConstantOperand c) = c
    getConstant _ = error "Only constants allowed inside tuples."

-- Variables
genOperand (Var (Name nameString)) localVars = do
  -- if localVars has it then it's a local reference otherwise mark it as a global reference
  -- local variable names end in "_number" so we need to take that into consideration
  -- also local variable names can have "_"
  case getLocalVarName nameString localVars of
    Just (_, localVar) -> return localVar
    Nothing -> do
      currentDefs <- liftModuleState $ gets builderDefs
      let maybeDef = getConstantFromDefs currentDefs (Name nameString)
      case maybeDef of
        Just def -> do
          case def of
            (GlobalDefinition AST.GlobalVariable {G.type' = t}) -> load (ConstantOperand (C.GlobalReference (ASTType.ptr t) (Name nameString))) 0
            _ -> error $ "Constant " <> show nameString <> " not found."
        Nothing -> error $ "Constant " <> show nameString <> " not found."

-- Call
genOperand (S.Call (Name fnName) functionArgs) localVars = do
  largs <- mapM (`genOperand` localVars) functionArgs
  let functionDefinition = getLocalVarName fnName localVars
  case functionDefinition of
    Just (_, localVar) -> call localVar (map (\x -> (x, [])) largs)
    Nothing -> do
      currentDefs <- liftModuleState $ gets builderDefs
      let maybeDef = getFunctionFromDefs currentDefs (Name fnName)
      case maybeDef of
        Just def -> do
          case def of
            (GlobalDefinition AST.Function {G.returnType = retT, G.parameters = params}) -> call (getFunctionOperand (Name fnName) retT params) (map (\x -> (x, [])) largs)
            _ -> error $ "Function " <> show fnName <> " not found."
        Nothing -> error $ "Function " <> show fnName <> " not found."

-- Unary Operands (Prefix Operands)
genOperand (UnaryOp oper a) localVars = do
  op <- genOperand a localVars
  currentDefs <- liftModuleState $ gets builderDefs
  case M.lookup oper (unops currentDefs) of
    Just f -> f op
    Nothing -> error "This shouldn't have matched here, unary operand doesn't exist."
  where
    unops :: SnocList Definition -> M.Map ShortByteString (AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    unops defs =
      M.fromList
        [
          ("-", fneg),
          ("!", not'),
          ("fst", \x -> tupleAccessorOperand x (ConstantOperand (C.Int 32 0))),
          ("snd", \x -> tupleAccessorOperand x (ConstantOperand (C.Int 32 1))),
          ("tail", \x -> do
            i32_slot <- gep x [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
            load i32_slot 0),
          ("head", \x -> do
            i32_slot <- gep x [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
            load i32_slot 0)
        ]
      where
        not' :: AST.Operand -> IRBuilderT ModuleBuilder AST.Operand
        not' x = do
          icmp IP.EQ x (ConstantOperand (C.Int 1 0))

-- Binary Operands (Infix Operands)
genOperand (BinOp oper a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  case M.lookup oper $ binops opA opB of
    Just f -> f opA opB
    Nothing -> genOperand (S.Call (Name ("binary_" <> oper)) [a, b]) localVars
  where
    binops :: AST.Operand -> AST.Operand -> M.Map ShortByteString (AST.Operand -> AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    binops firstOp secondOp =
      -- TODO: This info is also in the ParserH binops, is it necessary for it to be there?
      M.fromList
        [ ("+", eitherType add fadd),
          ("-", eitherType sub fsub),
          ("*", eitherType mul fmul),
          ("/", eitherType udiv fdiv),
          ("<", eitherType (icmp IP.ULT) (fcmp ULT)),
          ("<=", eitherType (icmp IP.ULE) (fcmp ULE)),
          (">", eitherType (icmp IP.UGT) (fcmp UGT)),
          (">=", eitherType (icmp IP.UGE) (fcmp UGE)),

          ("==", eitherType (icmp IP.EQ) (fcmp UEQ)),
          ("!=", eitherType (icmp IP.NE) (fcmp UNE)),

          ("^^", LLVM.IRBuilder.Instruction.xor),
          ("&&", LLVM.IRBuilder.Instruction.and),
          ("||", LLVM.IRBuilder.Instruction.or)
        ]
      where
        eitherType = typedOperandInstruction firstOp secondOp

-- If
genOperand (If cond thenExpr elseExpr) localVars = mdo
  computedCond <- genOperand cond localVars
  condBr computedCond ifThen ifElse
  ifThen <- block `named` "if.then"
  computedThen <- genOperand thenExpr localVars
  br ifExit
  ifElse <- block `named` "if.else"
  computedElse <- genOperand elseExpr localVars
  br ifExit
  ifExit <- block `named` "if.exit"
  phi [(computedThen, ifThen), (computedElse, ifElse)]

-- Let in
genOperand (Let Double (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.double Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  -- TODO: alloca -> store -> load: there's probably a better way to do this
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand (Let Integer (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.i32 Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand (Let Boolean (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.i1 Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand (Let (Tuple t1 t2) (Name varName) variableValue body) localVars = do
  var <- alloca (ASTType.StructureType False [getASTType t1, getASTType t2]) Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)

-- Lists
genOperand (List []) _ = nullIntList -- TODO: No
-- genOperand (List [x]) localVars = do
--     var <- alloca intListType Nothing 0
--     i32_slot <- gep var [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
--     computedValue <- genOperand x localVars
--     store i32_slot 0 computedValue
--     null_slot <- gep var [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
--     store null_slot 0 (ConstantOperand $ Null intListPtrType)
--     return var
--   where
--     intListType = ASTType.NamedTypeReference (AST.Name "IntList")
--     intListPtrType = ASTType.PointerType intListType (AST.AddrSpace 0)
genOperand (List (x:xs)) localVars = do
    var <- alloca intListType Nothing 0 -- TODO: too many allocas -- TODO: only int list
    i32_slot <- gep var [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
    nodeValue <- genOperand x localVars
    store i32_slot 0 nodeValue
    next_slot <- gep var [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
    nextValue <- genOperand (List xs) localVars
    store next_slot 0 nextValue
    return var
  where
    elementType = getExpressionType x [] -- TODO: local vars
    intListType = ASTType.NamedTypeReference (AST.Name $ fromString $ listPointerTypeName x)
    intListPtrType = ASTType.PointerType intListType (AST.AddrSpace 0)
  -- do
  -- leftOp <- trace ("operand: " ++ show x) $ genOperand x localVars
  -- rightOp <- genOperand (List xs) localVars
  -- return $ ConstantOperand (C.Struct {C.structName = Nothing, C.isPacked = False, C.memberValues = [getConstant leftOp, getConstant rightOp]})
  -- where
  --   getConstant (ConstantOperand c) = c -- TODO: DRY
  --   getConstant _ = error "Only constants allowed inside tuples."



genOperand x _ = error $ "This shouldn't have matched here: " <> show x