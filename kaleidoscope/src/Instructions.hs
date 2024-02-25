module Instructions where

import Syntax (Expr)
import Types (getExpressionType)

import qualified LLVM.AST.Type as ASTType
import LLVM.AST ( Operand (LocalReference, ConstantOperand, MetadataOperand) )
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Module (ModuleBuilder)
import LLVM.IRBuilder.Instruction
import qualified Data.Kind as AST
import LLVM.AST.Constant

type BinOpInstruction = (Operand -> Operand -> IRBuilderT ModuleBuilder Operand)

typedInstruction :: Expr -> Expr -> BinOpInstruction -> BinOpInstruction -> BinOpInstruction
typedInstruction a b wholeInstr floatingInstr = do
  let aType = getExpressionType a
  let bType = getExpressionType b
  -- TODO: make this a case statement :/
  -- "Qualified name in binding position: ASTType.double"
  if aType == ASTType.i32 && bType == ASTType.i32
    then wholeInstr
  else if aType == ASTType.double && bType == ASTType.double
    then floatingInstr
  else if aType == ASTType.i32 && bType == ASTType.double
    then \x y -> do
      x' <- sitofp x ASTType.double
      floatingInstr x' y
  else if aType == ASTType.double && bType == ASTType.i32
    then \x y -> do
      y' <- uitofp y ASTType.double
      floatingInstr x y'
  else error "Invalid types for operand"

typedOperandInstruction :: Operand -> Operand -> BinOpInstruction -> BinOpInstruction -> BinOpInstruction
typedOperandInstruction a b wholeInstr floatingInstr = do
  let aType = operandType a
  let bType = operandType b
  -- TODO: make this a case statement :/
  -- "Qualified name in binding position: ASTType.double"
  if aType == ASTType.i32 && bType == ASTType.i32
    then wholeInstr
  else if aType == ASTType.double && bType == ASTType.double
    then floatingInstr
  else if aType == ASTType.i32 && bType == ASTType.double
    then \x y -> do
      x' <- sitofp x ASTType.double
      floatingInstr x' y
  else if aType == ASTType.double && bType == ASTType.i32
    then \x y -> do
      y' <- uitofp y ASTType.double
      floatingInstr x y'
  else error "Invalid types for operand"

operandType :: Operand -> ASTType.Type
operandType op = case op of
  LocalReference ty _ -> ty
  ConstantOperand con -> case con of
    Int _ _-> ASTType.i32
    Float _ -> ASTType.double
    _ -> ASTType.double -- TODO
  MetadataOperand _ -> ASTType.double -- TODO
