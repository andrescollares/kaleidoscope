{-# LANGUAGE OverloadedStrings #-}
module List where

import LLVM.IRBuilder
import qualified LLVM.AST.Operand as ASTOperand
import qualified LLVM.AST.Type as ASTType
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace
import LLVM.AST.Constant (Constant(Null))
import LLVM.AST (Operand(ConstantOperand))

nullIntList :: IRBuilderT ModuleBuilder ASTOperand.Operand
nullIntList = do
    let intListType = ASTType.NamedTypeReference (AST.Name "IntList")
    let intListPtrType = ASTType.PointerType intListType (AddrSpace 0)
    let listn't = Null intListPtrType
    return $ ConstantOperand listn't