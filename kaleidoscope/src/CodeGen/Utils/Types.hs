{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Utils.Types where

import Data.List (find)
import qualified LLVM.AST as AST
import LLVM.AST.AddrSpace (AddrSpace (AddrSpace))
import qualified LLVM.AST.Constant as AST.Constant
import qualified LLVM.AST.Type as ASTType
import qualified Syntax as S

type CurrentDef = (AST.Name, S.Type)

-- TODO: probably missing some cases
astTypeToSyntaxType :: ASTType.Type -> S.Type
astTypeToSyntaxType t = case t of
  ASTType.FloatingPointType {ASTType.floatingPointType = ASTType.DoubleFP} -> S.Double
  ASTType.IntegerType {ASTType.typeBits = 32} -> S.Integer
  ASTType.IntegerType {ASTType.typeBits = 1} -> S.Boolean
  ASTType.StructureType {ASTType.elementTypes = [t1, t2]} -> S.Tuple (astTypeToSyntaxType t1) (astTypeToSyntaxType t2)
  ASTType.VoidType -> S.Void
  _ -> error $ "Unsupported type " ++ show t

syntaxTypeToASTType :: S.Type -> AST.Type
syntaxTypeToASTType S.Double = ASTType.double
syntaxTypeToASTType S.Integer = ASTType.i32
syntaxTypeToASTType S.Boolean = ASTType.i1
syntaxTypeToASTType (S.Tuple t1 t2) = ASTType.PointerType (ASTType.StructureType False [syntaxTypeToASTType t1, syntaxTypeToASTType t2]) (AddrSpace 0)
syntaxTypeToASTType (S.ListType t) = ASTType.PointerType (ASTType.StructureType False [syntaxTypeToASTType t, ASTType.ptr ASTType.void]) (AddrSpace 0)
syntaxTypeToASTType (S.FunType argTypes retType) = ASTType.ptr (ASTType.FunctionType {ASTType.resultType = syntaxTypeToASTType retType, ASTType.argumentTypes = map syntaxTypeToASTType argTypes, ASTType.isVarArg = False})
syntaxTypeToASTType S.Void = ASTType.VoidType

getExpressionType :: S.Expr -> [CurrentDef] -> AST.Type
getExpressionType (S.Int _) _ = ASTType.i32
getExpressionType (S.Float _) _ = ASTType.double
getExpressionType (S.Bool _) _ = ASTType.i1
getExpressionType (S.Call functionName _) currentDefs = syntaxTypeToASTType $ findLocalVarType currentDefs functionName
getExpressionType (S.Var varName) currentDefs = syntaxTypeToASTType $ findLocalVarType currentDefs varName
getExpressionType (S.UnaryOp unOp (S.TupleI e1 e2)) currentDefs =
  case unOp of
    "fst" -> getExpressionType e1 currentDefs
    "snd" -> getExpressionType e2 currentDefs
    _ -> error "Unsupported unary operation on Tuple"
getExpressionType (S.UnaryOp unOp (S.List (x : xs))) currentDefs =
  case unOp of
    "head" -> getExpressionType x currentDefs
    "tail" -> getExpressionType (S.List xs) currentDefs
    _ -> error "Unsupported unary operation on List"
getExpressionType (S.UnaryOp unOp e) currentDefs =
  case unOp of
    "!" -> ASTType.i1
    "-" -> getExpressionType e currentDefs
    _ -> getExpressionType e currentDefs
-- TODO: this is potentially O(2^n)!!!
getExpressionType (S.BinOp name a b) currentDefs
  | name == ":" = typeOfB
  | name `elem` ["==", "!=", "<", ">", "<=", ">="] = ASTType.i1
  | typeOfA == ASTType.double || typeOfB == ASTType.double = ASTType.double
  | otherwise = typeOfA
  where
    typeOfA = getExpressionType a currentDefs
    typeOfB = getExpressionType b currentDefs
-- FIXME: fix this crappy current defs handling
getExpressionType (S.Let varName varExpr e) currentDefs = getExpressionType e $ currentDefs ++ [(varName, astTypeToSyntaxType $ getExpressionType varExpr currentDefs)]
getExpressionType (S.If _ e1 e2) currentDefs =
  if e1Type == getExpressionType e2 currentDefs
    then e1Type
    else error "Types of both sides of if statement should be the same"
  where
    e1Type = getExpressionType e1 currentDefs
getExpressionType (S.List (x : rest)) currentDefs = do
  ASTType.PointerType (ASTType.StructureType False [getExpressionType x currentDefs, getExpressionType (S.List rest) currentDefs]) (AddrSpace 0)
getExpressionType (S.List []) _ = ASTType.PointerType ASTType.VoidType (AddrSpace 0)
getExpressionType (S.TupleI e1 e2) currentDefs = ASTType.PointerType (ASTType.StructureType False [getExpressionType e1 currentDefs, getExpressionType e2 currentDefs]) (AddrSpace 0)

findLocalVarType :: [CurrentDef] -> AST.Name -> S.Type
findLocalVarType currentDefs varName = case find (\(n, _) -> n == varName) currentDefs of
  Just (_, t) -> t
  Nothing -> error $ "Cannot determine type of variable " ++ show varName

operandType :: AST.Operand -> AST.Type
operandType op = case op of
  AST.LocalReference ty _ -> ty
  AST.ConstantOperand con -> case con of
    AST.Constant.Int 32 _ -> ASTType.i32
    AST.Constant.Int 1 _ -> ASTType.i1
    AST.Constant.Float _ -> ASTType.double
    AST.Constant.Null ty -> ASTType.PointerType ty (AddrSpace 0)
    _ -> error $ "Unsupported constant operand type: " ++ show op
  _ -> error $ "Unsupported operand type: " ++ show op