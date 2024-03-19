{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module IRBuilder.GenOperand where


import Control.Monad.RWS (gets)
import Data.Bifunctor (first)
import Data.ByteString.Short
import qualified Data.Map.Strict as M
import Data.String
import qualified Data.Text as T
import JIT
import Types
import Instructions
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (UEQ, UGE, UGT, ULE, ULT, UNE))
import qualified LLVM.AST.IntegerPredicate as IP
import LLVM.AST.Global (Global (name, GlobalVariable, type'), parameters, returnType)
import qualified LLVM.AST.Global as G (Global (name, type'))
import LLVM.AST.Type (ptr)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Internal.SnocList
import LLVM.IRBuilder.Module (ModuleBuilder, ModuleBuilderState (ModuleBuilderState, builderDefs, builderTypeDefs), MonadModuleBuilder (liftModuleState), execModuleBuilder, extern, function, global, ParameterName (ParameterName))
import LLVM.IRBuilder.Monad
import Syntax as S
import qualified Data.List as DL
import IRBuilder.LocalVar
    ( LocalVar,
      getLocalVarName,
      getFunctionFromDefs,
      getConstantFromDefs,
      getFunctionOperand )

-- Generates the Operands that genTopLevel needs.
genOperand :: S.Operand -> [LocalVar] -> IRBuilderT ModuleBuilder AST.Operand
-- Float
genOperand (Float n) _ = return $ ConstantOperand (C.Float (F.Double n))
-- Integer
genOperand (Int n) _ = return $ ConstantOperand (C.Int 32 n)
-- Bool
genOperand (Bool b) _ = return $ ConstantOperand (C.Int 1 (if b then 1 else 0))
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
            (GlobalDefinition AST.Function {returnType = retT, parameters = params}) -> call (getFunctionOperand (Name fnName) retT params) (map (\x -> (x, [])) largs)
            _ -> error $ "Function " <> show fnName <> " not found."
        Nothing -> error $ "Function " <> show fnName <> " not found."

-- Unary Operands
genOperand (UnaryOp oper a) localVars = do
  op <- genOperand a localVars
  case M.lookup oper unops of
    Just f -> f op
    Nothing -> error "This shouldn't have matched here, unary operand doesn't exist."
  where
    unops :: M.Map ShortByteString (AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    unops =
      M.fromList
        [("-", fneg)]

-- Binary Operands
genOperand (BinOp oper a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  case M.lookup oper $ binops opA opB of
    Just f -> f opA opB
    Nothing -> genOperand (S.Call (Name ("binary_" <> oper)) [a, b]) localVars
  where
    binops :: AST.Operand -> AST.Operand -> M.Map ShortByteString (AST.Operand -> AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    binops firstOp secondOp =
      M.fromList
        [ ("+", eitherType add fadd),
          ("-", eitherType sub fsub),
          ("*", eitherType mul fmul),
          ("/", eitherType udiv fdiv),
          ("<", eitherType (icmp IP.ULT) (fcmp ULT)),
          (">", eitherType (icmp IP.UGT) (fcmp UGT)),
          ("==", eitherType (icmp IP.EQ) (fcmp UEQ)),
          ("!=", eitherType (icmp IP.NE) (fcmp UNE)),
          ("<=", eitherType (icmp IP.ULE) (fcmp ULE)),
          (">=", eitherType (icmp IP.UGE) (fcmp UGE))
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
genOperand x _ = error $ "This shouldn't have matched here: " <> show x