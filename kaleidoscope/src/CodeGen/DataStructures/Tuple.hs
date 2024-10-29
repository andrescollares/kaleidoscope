{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module CodeGen.DataStructures.Tuple where

import CodeGen.Utils.Types (operandType)
import LLVM.AST (Operand (ConstantOperand))
import LLVM.IRBuilder (IRBuilderT, ModuleBuilder, alloca, gep, load, store)
import LLVM.AST.Constant (Constant(Int))

tupleAccessorOperand :: Operand -> Operand -> IRBuilderT ModuleBuilder Operand
tupleAccessorOperand tupleOperand indexOperand = do
  var <- alloca tupleType Nothing 8
  store var 0 tupleOperand
  tmp_input_w0 <- gep var [ConstantOperand (Int 32 0), indexOperand]
  load tmp_input_w0 8
  where
    tupleType = operandType tupleOperand
