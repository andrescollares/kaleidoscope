module Types where

import Syntax
import qualified LLVM.AST.Type as ASTType

getExpressionType :: Expr -> ASTType.Type
getExpressionType (Integer _) = ASTType.i32
getExpressionType _ = ASTType.double