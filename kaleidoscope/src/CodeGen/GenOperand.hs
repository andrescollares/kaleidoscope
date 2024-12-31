{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.GenOperand where

import CodeGen.DataStructures.List (createListNode, nullIntList, prependNode)
import CodeGen.DataStructures.Tuple (tupleAccessorOperand)
import CodeGen.LocalVar
  ( LocalVar,
    getConstantFromDefs,
    getFunctionFromDefs,
    getFunctionOperand,
    getLocalVarName,
  )
import CodeGen.Utils.Types (getASTType, operandType)
import Control.Monad.RWS (gets)
import qualified Data.Map.Strict as M
import LLVM.AST as AST
  ( Definition (GlobalDefinition),
    Global (Function, GlobalVariable),
    Name (Name),
    Operand (ConstantOperand),
    Type (..),
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
import qualified Syntax as S

-- Generates the Operands that genTopLevel needs.
genOperand :: S.Expr -> [LocalVar] -> IRBuilderT ModuleBuilder AST.Operand
-- Float
genOperand (S.Float n) _ = return $ ConstantOperand (C.Float (F.Double n))
-- Integer
genOperand (S.Int n) _ = return $ ConstantOperand (C.Int 32 n)
-- Bool
genOperand (S.Bool b) _ = return $ ConstantOperand (C.Int 1 (if b then 1 else 0))
-- Tuple
genOperand (S.TupleI a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  return $ ConstantOperand (C.Struct {C.structName = Nothing, C.isPacked = False, C.memberValues = [getConstant opA, getConstant opB]})
  where
    getConstant (ConstantOperand c) = c
    getConstant _ = error "Only constants allowed inside tuples."

-- Variables
genOperand (S.Var (Name nameString)) localVars = do
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
            (GlobalDefinition AST.Function {G.returnType = retT, G.parameters = params}) -> return $ getFunctionOperand (Name nameString) retT params
            _ -> error $ "Constant " <> show nameString <> " not found as global variable." <> show def
        Nothing -> error $ "Constant " <> show nameString <> " not found." <> show localVars

-- Call
-- functionArgs = [Int, Double, Bool, Tuple, List, (FunOp Name)]
genOperand (S.Call (Name fnName) functionArgs) localVars = do
  largs <- mapM (`genOperand` localVars) functionArgs
  -- Only match if fnName is same name as the current function
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
genOperand (S.UnaryOp oper a) localVars = do
  op <- genOperand a localVars
  case M.lookup oper unops of
    Just f -> f op
    Nothing -> error $ "Unary operation not defined: " <> show oper
  where
    unops :: M.Map Name (AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    unops =
      M.fromList
        [ ("-", fneg),
          ("!", not'),
          ("fst", \x -> tupleAccessorOperand x (ConstantOperand (C.Int 32 0))),
          ("snd", \x -> tupleAccessorOperand x (ConstantOperand (C.Int 32 1))),
          ( "tail",
            \x -> do
              i32_slot <- gep x [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
              load i32_slot 0
          ),
          ( "head",
            \x -> do
              i32_slot <- gep x [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
              load i32_slot 0
          )
        ]
      where
        not' :: AST.Operand -> IRBuilderT ModuleBuilder AST.Operand
        not' x = do
          icmp IP.EQ x (ConstantOperand (C.Int 1 0))

-- Binary Operands (Infix Operands)
genOperand (S.BinOp ":" a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  firstElem <- createListNode opA
  prependNode firstElem opB
genOperand (S.BinOp oper a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars
  case M.lookup oper $ binops opA opB of
    Just f -> f opA opB
    Nothing -> error $ "Binary operation not defined: " <> show oper
  where
    binops :: AST.Operand -> AST.Operand -> M.Map Name (AST.Operand -> AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)
    binops firstOp secondOp =
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
genOperand (S.If cond thenExpr elseExpr) localVars = mdo
  computedCond <- genOperand cond localVars
  condBr computedCond ifThen ifElse
  ifThen <- block `named` "if.then"
  computedThen <- genOperand thenExpr localVars
  br ifExit
  ifElse <- block `named` "if.else"
  computedElse <- genOperand elseExpr localVars
  br elseExit
  ifExit <- block `named` "if.if_exit"
  br blockEnd
  elseExit <- block `named` "if.else_exit"
  br blockEnd
  blockEnd <- block `named` "if.end"
  phi [(computedThen, ifExit), (computedElse, elseExit)]

-- Let in
genOperand (S.Let S.Double (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.double Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand (S.Let S.Integer (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.i32 Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand (S.Let S.Boolean (Name varName) variableValue body) localVars = do
  var <- alloca ASTType.i1 Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)
genOperand (S.Let (S.Tuple t1 t2) (Name varName) variableValue body) localVars = do
  var <- alloca (ASTType.StructureType False [getASTType t1, getASTType t2]) Nothing 0
  computedValue <- genOperand variableValue localVars
  store var 0 computedValue
  loadedVar <- load var 0
  genOperand body ((Just varName, loadedVar) : localVars)

-- Lists
genOperand (S.List []) _ = nullIntList
genOperand (S.List (x : xs)) localVars = do
  firstElem <- genOperand x localVars
  var <- createListNode firstElem
  next_slot <- gep var [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
  nextValue <- genOperand (S.List xs) localVars
  store next_slot 0 nextValue
  return var

-- Reference to a Function
genOperand (S.FunOp (Name fnName)) localVars = do
  currentDefs <- liftModuleState $ gets builderDefs
  let maybeDef = getFunctionFromDefs currentDefs (Name fnName)
  case maybeDef of
    Just def -> do
      case def of
        -- (GlobalDefinition AST.Function {G.returnType = retT, G.parameters = params}) -> return $ getFunctionOperand (Name fnName) retT params
        _ -> error $ "Function " <> show fnName <> " not found."
    Nothing -> error $ "Function " <> show fnName <> " not found."
genOperand x _ = error $ "This shouldn't have matched here: " <> show x

type BinOpInstruction = (AST.Operand -> AST.Operand -> IRBuilderT ModuleBuilder AST.Operand)

typedOperandInstruction :: AST.Operand -> AST.Operand -> BinOpInstruction -> BinOpInstruction -> BinOpInstruction
typedOperandInstruction a b wholeInstr floatingInstr = do
  let aType = operandType a
  let bType = operandType b
  case aType of
    (IntegerType _) -> case bType of
      (IntegerType _) -> wholeInstr
      (FloatingPointType _) -> \x y -> do
        x' <- sitofp x ASTType.double
        floatingInstr x' y
      _ -> error $ "Invalid types for operand: " ++ show aType ++ " and " ++ show bType
    (FloatingPointType _) -> case bType of
      (IntegerType 1) -> \x y -> do
        x' <- fptosi x ASTType.double
        floatingInstr x' y
      (IntegerType _) -> \x y -> do
        y' <- sitofp y ASTType.double
        floatingInstr x y'
      (FloatingPointType _) -> floatingInstr
      _ -> error $ "Invalid types for operand: " ++ show aType ++ " and " ++ show bType
    (ASTType.PointerType _ _) -> case bType of
      (ASTType.PointerType _ _) -> wholeInstr
      _ -> error "Pointers can only be compared to other pointers"
    _ -> error $ "Invalid types for operand: " ++ show aType ++ " and " ++ show bType