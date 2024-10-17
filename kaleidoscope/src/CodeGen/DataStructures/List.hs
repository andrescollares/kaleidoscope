{-# LANGUAGE OverloadedStrings #-}
module CodeGen.DataStructures.List where

import LLVM.IRBuilder
import qualified LLVM.AST.Operand as ASTOperand
import qualified LLVM.AST.Type as ASTType
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant (Constant(Null))
import LLVM.AST (Operand(ConstantOperand), Name (Name))
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.AddrSpace as AST
import Data.ByteString.Short (ShortByteString)
import Data.String
import CodeGen.Utils.Types (operandType)

nullIntList :: IRBuilderT ModuleBuilder ASTOperand.Operand
nullIntList = do
    let intListType = ASTType.NamedTypeReference (AST.Name "IntList")
    let intListPtrType = ASTType.PointerType intListType (AddrSpace 0)
    let listn't = Null intListPtrType
    return $ ConstantOperand listn't

createListNode :: AST.Operand -> IRBuilderT ModuleBuilder AST.Operand
createListNode nodeVal = do
  var <- call (ConstantOperand (C.GlobalReference (ASTType.ptr (ASTType.FunctionType listPtrType [] False)) (Name $ allocListNode elementType))) []
  i32_slot <- gep var [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 0)]
  store i32_slot 0 nodeVal
  return var
  where
    elementType = operandType nodeVal
    listType = ASTType.NamedTypeReference (AST.Name $ fromString $ listPointerTypeNameLLVM elementType)
    listPtrType = ASTType.PointerType listType (AST.AddrSpace 0)

listPointerTypeNameLLVM :: AST.Type -> String
listPointerTypeNameLLVM ASTType.IntegerType { ASTType.typeBits = 32 } = "IntList"
listPointerTypeNameLLVM ASTType.IntegerType { ASTType.typeBits = 1 } = "BoolList"
listPointerTypeNameLLVM (ASTType.FloatingPointType _) = "FloatList"
listPointerTypeNameLLVM x = error "Unsupported list element type: " <> show x

prependNode :: AST.Operand -> AST.Operand -> IRBuilderT ModuleBuilder AST.Operand
prependNode node list = do
  i32_slot <- gep node [ConstantOperand (C.Int 32 0), ConstantOperand (C.Int 32 1)]
  store i32_slot 0 list
  return node

allocListNode :: ASTType.Type -> ShortByteString
allocListNode elementType = case elementType of
      ASTType.FloatingPointType _ -> "_alloc_double_list_node"
      ASTType.IntegerType 1 -> "_alloc_bool_list_node"
      ASTType.IntegerType _ -> "_alloc_int_list_node"
      _ -> error $ "Cannot allocate list node for type: " <> show elementType