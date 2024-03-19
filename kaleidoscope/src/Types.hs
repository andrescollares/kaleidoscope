module Types where

import Data.List (find)
import LLVM.AST as AST ( Name, Type )
import qualified LLVM.AST.Type as ASTType
import Syntax as S

type LocalVarType = (Name, S.Type)

getExpressionType :: S.Operand -> [LocalVarType] -> AST.Type
getExpressionType (Int _) _ = ASTType.i32
getExpressionType (Float _) _ = ASTType.double
getExpressionType (Bool _) _ = ASTType.i1
-- getExpressionType (S.Call _ _) = We can't infer this without context
getExpressionType (Var varName) localVars = getASTType $ findLocalVarType localVars varName
getExpressionType (UnaryOp _ _) _ = ASTType.double -- TODO!!
-- TODO: this is potentially O(2^n)!!!
getExpressionType (BinOp _ a b) localVars =
  if typeOfA == ASTType.double || typeOfB == ASTType.double
    then ASTType.double
    else typeOfA
  where
    typeOfA = getExpressionType a localVars
    typeOfB = getExpressionType b localVars
getExpressionType (Let varType varName _ e) localVars = getExpressionType e $ localVars ++ [(varName, varType)]
getExpressionType (If _ e1 e2) localVars =
  if e1Type == getExpressionType e2 localVars
    then e1Type
    else error "Types of both sides of if statement should be the same"
  where
    e1Type = getExpressionType e1 localVars
getExpressionType _ _ = ASTType.double

getASTType :: S.Type -> AST.Type
getASTType Double = ASTType.double
getASTType Integer = ASTType.i32
getASTType Boolean = ASTType.i1

findLocalVarType :: [LocalVarType] -> Name -> S.Type
findLocalVarType localVars varName = case find (\(n, _) -> n == varName) localVars of
  Just (var, t) -> t
  Nothing -> error $ "Cannot determine type of variable " ++ show varName