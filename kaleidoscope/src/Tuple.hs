{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Tuple where

import LLVM.AST
import LLVM.AST.Global hiding (functionAttributes, returnAttributes, callingConvention, alignment, metadata)
import LLVM.AST.Type
import qualified LLVM.AST.Constant as C
import Data.String
import Data.List ( intercalate )

import Types (structType, getExpressionType)
import LLVM.IRBuilder
import qualified LLVM.AST as AST

tupleAccessorOperand :: Operand -> Operand -> Type -> IRBuilderT ModuleBuilder AST.Operand
tupleAccessorOperand tupleOperand indexOperand tupleType = do
  var <- alloca tupleType Nothing 8
  store var 0 tupleOperand
  tmp_input_w0 <- gep var [ConstantOperand (C.Int 32 0), indexOperand]
  load tmp_input_w0 8


-- tupleAccessor :: Integer -> [Type] -> Definition
-- tupleAccessor tupleIndex types =
--       GlobalDefinition
--       functionDefaults
--         { name = Name (fromString $ "_accessor_" ++ intercalate "_" (map typeToString types) ++ "_" ++ show tupleIndex),
--           parameters = ([Parameter (structType types) (Name (fromString "targetStruct")) []], False),
--           returnType = IntegerType 32,
--           basicBlocks = [
--                    BasicBlock (Name $ fromString "entry") [
--                     -- GetElementPtr expects a pointer to the struct, the next 2 steps are to get the pointer to the tuple element
--                     UnName 0 := Alloca {
--                       allocatedType = structType types,
--                       numElements = Nothing,
--                       alignment = 8,
--                       metadata = []
--                     },
--                     Do $ Store {
--                       volatile = False,
--                       address = LocalReference (ptr (structType types)) (UnName 0),
--                       value = LocalReference (structType types) (Name (fromString "targetStruct")),
--                       maybeAtomicity = Nothing,
--                       alignment = 8,
--                       metadata = []
--                     },
--                     Name (fromString "tmp_input_w0") := GetElementPtr {
--                       inBounds = True,
--                       address = LocalReference (structType types) (UnName 0),
--                       indices = [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 tupleIndex)],
--                       metadata = []
--                     },
--                     UnName 1 := Load {
--                       volatile = False,
--                       address = LocalReference (ptr (FloatingPointType DoubleFP)) (Name (fromString "tmp_input_w0")),
--                       maybeAtomicity = Nothing,
--                       alignment = 8,
--                       metadata = []
--                     }
--                     ] (
--                       Do $ Ret (Just (LocalReference (IntegerType 32) (UnName 1))) []
--                     )
--                   ]
--         }

typeToString :: Type -> String
typeToString (IntegerType 32) = "i32"
typeToString (FloatingPointType DoubleFP) = "double"
typeToString (IntegerType 1) = "bool"
typeToString _ = error "Unsupported type"
