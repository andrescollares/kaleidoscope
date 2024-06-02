module Instructions where

import LLVM.AST (Operand (ConstantOperand, LocalReference), Type (IntegerType, FloatingPointType, elementTypes, StructureType))
import LLVM.AST.Constant ( Constant(Float, Int, Struct, memberValues) )
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
      _ -> error $ "Invalid types for operand: " ++ (show aType) ++ " and " ++ (show bType)
    (FloatingPointType _) -> case bType of
      (IntegerType 1) -> \x y -> do
        x' <- fptosi x ASTType.double
        floatingInstr x' y
      (IntegerType _) -> \x y -> do
        y' <- sitofp y ASTType.double
        floatingInstr x y'
      (FloatingPointType _) -> floatingInstr
      _ -> error $ "Invalid types for operand: " ++ (show aType) ++ " and " ++ (show bType)
    _ -> error $ "Invalid types for operand: " ++ (show aType) ++ " and " ++ (show bType)

operandType :: Operand -> ASTType.Type
operandType op = case op of
  LocalReference ty _ -> ty
  ConstantOperand con -> case con of
    Int _ _ -> ASTType.i32
    Float _ -> ASTType.double
    Struct { memberValues = [a, b] } -> StructureType False [operandType $ ConstantOperand a, operandType $ ConstantOperand b]
    _ -> error $ "Unsupported constant operand type: " ++ show op
  _ -> error $ "Unsupported operand type: " ++ show op
