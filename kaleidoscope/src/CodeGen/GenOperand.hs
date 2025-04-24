{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.GenOperand where

import CodeGen.LocalVar
  ( LocalVar,
    getFunctionOperand,
    getLocalVarByName,
  )
import CodeGen.Utils.Types (operandType)
import Control.Monad.RWS (gets)
import qualified Data.Map.Strict as M
import Data.Word (Word32)
import LLVM.AST as AST
  ( Definition (GlobalDefinition),
    Global (Function),
    Instruction (Store),
    Name (Name),
    Operand (ConstantOperand),
    Type (..),
  )
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (UEQ, UGE, UGT, ULE, ULT, UNE))
import qualified LLVM.AST.Global as AST.Global
import qualified LLVM.AST.Global as G
import qualified LLVM.AST.IntegerPredicate as IP
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder (ModuleBuilder, MonadIRBuilder, builderDefs, emitInstrVoid, liftModuleState)
import LLVM.IRBuilder.Constant (bit, double, int32)
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Internal.SnocList (SnocList (SnocList))
import LLVM.IRBuilder.Monad (IRBuilderT, block, named)
import qualified Syntax as S
import qualified LLVM.AST.Constant as Constant

-- Generates the Operands that genTopLevel needs.
genOperand :: S.Expr -> [LocalVar] -> IRBuilderT ModuleBuilder AST.Operand

-- Constants
genOperand (S.Float n) _ = return $ double n
genOperand (S.Int n) _ = return $ int32 n
genOperand (S.Bool b) _ = return $ bit (if b then 1 else 0)

-- Variables
genOperand (S.Var (Name nameString)) localVars = do
  -- if localVars has it then it's a local reference otherwise mark it as a global reference
  -- local variable names end in "_number" so we need to take that into consideration
  -- also local variable names can have "_"
  case getLocalVarByName nameString localVars of
    Just (_, localVar) -> return localVar
    Nothing -> do
      currentDefs <- liftModuleState $ gets builderDefs
      let maybeDef = getFunctionFromDefs currentDefs (Name nameString)
      case maybeDef of
        Just (GlobalDefinition AST.Function {G.returnType = retT, G.parameters = params}) -> return $ getFunctionOperand (Name nameString) retT params
        Just def -> error $ "Constant " <> show nameString <> " not found as global variable." <> show def
        Nothing -> error $ "Constant " <> show nameString <> " not found." <> show localVars

-- Call
genOperand (S.Call (Name fnName) functionArgs) localVars = do
  largs <- mapM (`genOperand` localVars) functionArgs
  currentDefs <- liftModuleState $ gets builderDefs
  let maybeDef = getFunctionFromDefs currentDefs (Name fnName)
  case maybeDef of
    Just (GlobalDefinition AST.Function {G.returnType = retT, G.parameters = params}) ->
      call (getFunctionOperand (Name fnName) retT params) (map (\x -> (x, [])) largs)
    Just _ -> error $ "Function " <> show fnName <> " not found."
    Nothing -> do
      let localVar = getLocalVarByName fnName localVars
      case localVar of
        -- This only happens if we're in a high level function where there's a function as an attribute.
        Just (_, localFunctionVar) -> call localFunctionVar (map (\x -> (x, [])) largs)
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
        [ ( "-",
            \x -> case operandType x of
              (IntegerType _) -> sub (int32 0) x
              (FloatingPointType _) -> fsub (ConstantOperand (C.Float (F.Double 0))) x
              _ -> error $ "Invalid type for operand: " ++ show (operandType x)
          ),
          ("!", not'),
          ( "fst",
            \tuplePtr -> do
              tuple <- load tuplePtr 0
              extractValue tuple [0]
          ),
          ( "snd",
            \tuplePtr -> do
              tuple <- load tuplePtr 0
              extractValue tuple [1]
          ),
          ( "tail",
            \x -> do
              i32_slot <- gep x [int32 0, int32 1]
              load i32_slot 0
          ),
          ( "head",
            \x -> do
              i32_slot <- gep x [int32 0, int32 0]
              load i32_slot 0
          )
        ]
      where
        not' :: AST.Operand -> IRBuilderT ModuleBuilder AST.Operand
        not' x = icmp IP.EQ x (bit 0)
-- Binary Operands (Infix Operands)
genOperand (S.BinOp ":" x xs) localVars = do
  case xs of
    S.List _ -> do
      xOper <- genOperand x localVars
      xsOper <- genOperand xs localVars

      newListStruct <- alloca (ASTType.StructureType False [operandType xOper, operandType xsOper]) Nothing 0

      xPtr <- gep newListStruct [int32 0, int32 0]
      storeVolatile xPtr 0 xOper
      xsPtr <- gep newListStruct [int32 0, int32 1]
      storeVolatile xsPtr 0 xsOper
      
      return newListStruct
    _ -> error "Invalid list syntax"
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
          ("%", eitherType urem frem),
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
-- NOTE: Extra if_exit and else_exit labels are necessary
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
genOperand (S.Let (Name varName) variableValue body) localVars = do
  var <- genOperand variableValue localVars
  genOperand body ((Just varName, var) : localVars)
-- Tuple
genOperand (S.TupleI a b) localVars = do
  opA <- genOperand a localVars
  opB <- genOperand b localVars

  tupleStruct <- alloca (ASTType.StructureType False [operandType opA, operandType opB]) Nothing 0

  opAPtr <- gep tupleStruct [int32 0, int32 0]
  storeVolatile opAPtr 0 opA
  opBPtr <- gep tupleStruct [int32 0, int32 1]
  storeVolatile opBPtr 0 opB

  return tupleStruct
-- Lists
genOperand (S.List []) _ = return $ ConstantOperand $ Constant.Null (ASTType.ptr ASTType.VoidType)
genOperand (S.List (x : xs)) localVars = do
  firstElem <- genOperand x localVars
  rest <- genOperand (S.List xs) localVars

  listStruct <- alloca (ASTType.StructureType False [operandType firstElem, operandType rest]) Nothing 0

  firstElemPtr <- gep listStruct [int32 0, int32 0]
  storeVolatile firstElemPtr 0 firstElem
  restPtr <- gep listStruct [int32 0, int32 1]
  storeVolatile restPtr 0 rest

  return listStruct
genOperand x _ = error $ "This shouldn't have matched here: " <> show x

storeVolatile :: MonadIRBuilder m => Operand -> Word32 -> Operand -> m ()
storeVolatile addr align val = emitInstrVoid $ Store True addr val Nothing align []

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

getFunctionFromDefs :: SnocList Definition -> Name -> Maybe Definition
getFunctionFromDefs defs functionName = find (`matchNameGlobal` functionName) defs Nothing
  where
    find :: (a -> Bool) -> SnocList a -> Maybe a -> Maybe a
    find p (SnocList (x : xs)) res
      | p x = Just x
      | otherwise = find p (SnocList xs) res
    find _ (SnocList []) res = res

    matchNameGlobal :: Definition -> Name -> Bool
    matchNameGlobal (GlobalDefinition AST.Function {AST.Global.name = n}) nameToMatch = n == nameToMatch
    matchNameGlobal _ _ = False