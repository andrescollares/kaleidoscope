module Types where

import Syntax as S
import LLVM.AST as AST
import qualified LLVM.AST.Type as ASTType

getExpressionType :: Expr -> AST.Type
getExpressionType (Int _) = ASTType.i32
getExpressionType (Float _) = ASTType.double
getExpressionType (Bool _) = ASTType.i1
getExpressionType (Constant Double _ _) = ASTType.double
getExpressionType (Constant Integer _ _) = ASTType.i32
getExpressionType (Constant Boolean _ _) = ASTType.i1
getExpressionType (S.Call _ _) = ASTType.double -- TODO!!
getExpressionType (Var _) = ASTType.double -- TODO!!
getExpressionType (UnaryOp _ _) = ASTType.double -- TODO!!
getExpressionType (BinOp _ a b) = if getExpressionType a == ASTType.double || getExpressionType b == ASTType.double 
  then ASTType.double 
  else getExpressionType a
getExpressionType _ = ASTType.double


getASTType :: S.Type -> AST.Type
getASTType Double = ASTType.double
getASTType Integer = ASTType.i32
getASTType Boolean = ASTType.i1