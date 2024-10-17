{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module CodeGen.DataStructures.Tuple where

import CodeGen.Utils.Types (operandType)
import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder

tupleAccessorOperand :: Operand -> Operand -> IRBuilderT ModuleBuilder AST.Operand
tupleAccessorOperand tupleOperand indexOperand = do
  var <- alloca tupleType Nothing 8
  store var 0 tupleOperand
  tmp_input_w0 <- gep var [ConstantOperand (C.Int 32 0), indexOperand]
  load tmp_input_w0 8
  where
    tupleType = operandType tupleOperand

-- typeToString :: Type -> String
-- typeToString (IntegerType 32) = "i32"
-- typeToString (FloatingPointType DoubleFP) = "double"
-- typeToString (IntegerType 1) = "bool"
-- typeToString t = error "Unsupported type" ++ show t
