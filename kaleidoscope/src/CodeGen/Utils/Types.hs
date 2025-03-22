{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Utils.Types where

import Data.List (find)
import Data.String (fromString)
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
  ASTType.PointerType {ASTType.pointerReferent = ASTType.NamedTypeReference (AST.Name n)} -> case n of
    "IntList" -> S.ListType S.Integer
    "BoolList" -> S.ListType S.Boolean
    "FloatList" -> S.ListType S.Double
    _ -> error $ "Unsupported list type " ++ show n
  _ -> error $ "Unsupported type " ++ show t

getExpressionType :: S.Expr -> [CurrentDef] -> AST.Type
getExpressionType (S.Int _) _ = ASTType.i32
getExpressionType (S.Float _) _ = ASTType.double
getExpressionType (S.Bool _) _ = ASTType.i1
getExpressionType (S.Call functionName _) currentDefs = getASTType $ findLocalVarType currentDefs functionName
getExpressionType (S.Var varName) currentDefs = getASTType $ findLocalVarType currentDefs varName
getExpressionType (S.UnaryOp unOp (S.TupleI e1 e2)) currentDefs =
  case unOp of
    "fst" -> getExpressionType e1 currentDefs
    "snd" -> getExpressionType e2 currentDefs
    _ -> error "Unsupported unary operation on Tuple"
getExpressionType (S.UnaryOp unOp (S.List (x : _))) currentDefs =
  case unOp of
    "head" -> getExpressionType x currentDefs
    "tail" -> listPointerType x currentDefs
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
getExpressionType (S.List (x : _)) currentDefs = listPointerType x currentDefs
-- FIXME: (?) empty list defaults to int list
getExpressionType (S.List []) _ = ASTType.PointerType (ASTType.NamedTypeReference (AST.Name (fromString "IntList"))) (AddrSpace 0)
getExpressionType (S.TupleI e1 e2) currentDefs = ASTType.StructureType False [getExpressionType e1 currentDefs, getExpressionType e2 currentDefs]

findLocalVarType :: [CurrentDef] -> AST.Name -> S.Type
findLocalVarType currentDefs varName = case find (\(n, _) -> n == varName) currentDefs of
  Just (_, t) -> t
  Nothing -> error $ "Cannot determine type of variable " ++ show varName

listPointerType :: S.Expr -> [CurrentDef] -> AST.Type
listPointerType listElement currentDefs = case getExpressionType listElement currentDefs of -- TODO: local vars param
  ASTType.IntegerType {ASTType.typeBits = 32} -> ASTType.PointerType (ASTType.NamedTypeReference (AST.Name (fromString "IntList"))) (AddrSpace 0)
  ASTType.IntegerType {ASTType.typeBits = 1} -> ASTType.PointerType (ASTType.NamedTypeReference (AST.Name (fromString "BoolList"))) (AddrSpace 0)
  ASTType.FloatingPointType _ -> ASTType.PointerType (ASTType.NamedTypeReference (AST.Name (fromString "FloatList"))) (AddrSpace 0)
  _ -> error "Unsupported list element type"

getASTType :: S.Type -> AST.Type
getASTType S.Double = ASTType.double
getASTType S.Integer = ASTType.i32
getASTType S.Boolean = ASTType.i1
getASTType (S.Tuple t1 t2) = ASTType.PointerType (ASTType.StructureType False [getASTType t1, getASTType t2]) (AddrSpace 0)
getASTType (S.ListType t) = ASTType.PointerType (ASTType.NamedTypeReference (AST.Name (fromString $ listSyntaxPointerTypeName t))) (AddrSpace 0)
getASTType (S.FunType argTypes retType) = ASTType.ptr (ASTType.FunctionType {ASTType.resultType = getASTType retType, ASTType.argumentTypes = map getASTType argTypes, ASTType.isVarArg = False})

listSyntaxPointerTypeName :: S.Type -> String
listSyntaxPointerTypeName S.Integer = "IntList"
listSyntaxPointerTypeName S.Boolean = "BoolList"
listSyntaxPointerTypeName S.Double = "FloatList"
listSyntaxPointerTypeName x = error "Unsupported list element type: " <> show x

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