module Instructions where

import LLVM.AST (Operand (ConstantOperand, LocalReference, MetadataOperand), Type (IntegerType, FloatingPointType))
import LLVM.AST.Constant ( Constant(Float, Int) )
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Instruction ( sitofp, fptosi )
import LLVM.IRBuilder.Module (ModuleBuilder)
import LLVM.IRBuilder.Monad ( IRBuilderT )

type BinOpInstruction = (Operand -> Operand -> IRBuilderT ModuleBuilder Operand)

-- TODO: preconditon: Operands are only Float or Integer
typedOperandInstruction :: Operand -> Operand -> BinOpInstruction -> BinOpInstruction -> BinOpInstruction
typedOperandInstruction a b wholeInstr floatingInstr = do
  let aType = operandType a
  let bType = operandType b
  case aType of
    (IntegerType _) -> case bType of
      (IntegerType _) -> wholeInstr
      (FloatingPointType _) -> \x y -> do
        x' <- sitofp x ASTType.double
        floatingInstr x' y
      _ -> error "Invalid types for operand"
    (FloatingPointType _) -> case bType of
      (IntegerType 1) -> \x y -> do
        x' <- fptosi x ASTType.double
        floatingInstr x' y
      (IntegerType _) -> \x y -> do
        y' <- sitofp y ASTType.double
        floatingInstr x y'
      (FloatingPointType _) -> floatingInstr
      _ -> error "Invalid types for operand"
    _ -> error "Invalid types for operand"

operandType :: Operand -> ASTType.Type
operandType op = case op of
  LocalReference ty _ -> ty
  ConstantOperand con -> case con of
    Int _ _ -> ASTType.i32
    Float _ -> ASTType.double
    _ -> ASTType.i1 -- TODO
  MetadataOperand _ -> ASTType.double -- TODO
