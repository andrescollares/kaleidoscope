{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.String (fromString)
import Data.List (find)
import LLVM.AST as AST ( Name (Name), Type (PointerType, NamedTypeReference, IntegerType, FloatingPointType, VoidType, FunctionType), Definition (TypeDefinition) )
import qualified LLVM.AST.Type as ASTType
import Syntax as S
import LLVM.IRBuilder.Internal.SnocList (SnocList (SnocList))
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import LLVM.AST.Type (ptr)


type LocalVarType = (Name, S.Type)

getExpressionType :: S.Operand -> [LocalVarType] -> AST.Type
getExpressionType (Int _) _ = ASTType.i32
getExpressionType (Float _) _ = ASTType.double
getExpressionType (Bool _) _ = ASTType.i1
getExpressionType (TupleI e1 e2) localVars = ASTType.StructureType False [getExpressionType e1 localVars, getExpressionType e2 localVars]
getExpressionType (S.Call functionName _) localVars = getASTType $ findLocalVarType localVars functionName
getExpressionType (Var varName) localVars = getASTType $ findLocalVarType localVars varName
getExpressionType (UnaryOp unOp (TupleI e1 e2)) localVars =
  case unOp of
    "fst" -> getExpressionType e1 localVars
    "snd" -> getExpressionType e2 localVars
    _ -> error "Unsupported unary operation on Tuple"
getExpressionType (UnaryOp unOp (List (x:_))) localVars =
  case unOp of
    "head" -> getExpressionType x localVars
    "tail" -> listPointerType x localVars
    _ -> error "Unsupported unary operation on List"
getExpressionType (UnaryOp unOp e) localVars =
  case unOp of
    "!" -> ASTType.i1
    "-" -> getExpressionType e localVars
    _ -> getExpressionType e localVars

-- TODO: this is potentially O(2^n)!!!
getExpressionType (BinOp name a b) localVars
  | name == ":" = typeOfB
  | name `elem` ["==", "!=", "<", ">", "<=", ">="] = ASTType.i1
  | typeOfA == ASTType.double || typeOfB == ASTType.double = ASTType.double
  | otherwise = typeOfA
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
-- getExpressionType (List [x]) localVars = getExpressionType x localVars
getExpressionType (List (x:_)) localVars = listPointerType x localVars
-- FIXME: (?) empty list defaults to int list
getExpressionType (List []) _ = PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
getExpressionType (FunOp _) _ = PointerType VoidType (AddrSpace 0)
-- getExpressionType e localVars = error $ "Unsupported expression: " ++ show e ++ "Local vars: " ++ show localVars

listPointerType :: S.Operand -> [LocalVarType] -> AST.Type
listPointerType listElement localVars = case (getExpressionType listElement localVars) of -- TODO: local vars param
  IntegerType { ASTType.typeBits = 32 } -> PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
  IntegerType { ASTType.typeBits = 1 } -> PointerType (NamedTypeReference (Name (fromString "BoolList"))) (AddrSpace 0)
  FloatingPointType _ -> PointerType (NamedTypeReference (Name (fromString "FloatList"))) (AddrSpace 0)
  _ -> error "Unsupported list element type"

listPointerTypeName :: S.Operand -> String
listPointerTypeName listElement = case (getExpressionType listElement []) of
  IntegerType { ASTType.typeBits = 32 } -> "IntList"
  IntegerType { ASTType.typeBits = 1 } -> "BoolList"
  FloatingPointType _ -> "FloatList"
  _ -> error "Unsupported list element type: " <> show listElement

listPointerTypeNameLLVM :: AST.Type -> String
listPointerTypeNameLLVM (ASTType.IntegerType { ASTType.typeBits = 32 }) = "IntList"
listPointerTypeNameLLVM (ASTType.IntegerType { ASTType.typeBits = 1 }) = "BoolList"
listPointerTypeNameLLVM (ASTType.FloatingPointType _) = "FloatList"
listPointerTypeNameLLVM x = error "Unsupported list element type: " <> show x

listSyntaxPointerTypeName :: S.Type -> String
listSyntaxPointerTypeName Integer = "IntList"
listSyntaxPointerTypeName Boolean = "BoolList"
listSyntaxPointerTypeName Double = "FloatList"
listSyntaxPointerTypeName x = error "Unsupported list element type: " <> show x

getASTType :: S.Type -> AST.Type
getASTType Double = ASTType.double
getASTType Integer = ASTType.i32
getASTType Boolean = ASTType.i1
getASTType (Tuple t1 t2) = ASTType.StructureType False [getASTType t1, getASTType t2]
getASTType (ListType t) = PointerType (NamedTypeReference (Name (fromString $ listSyntaxPointerTypeName t))) (AddrSpace 0)
getASTType (FunType argTypes retType) = ptr ( FunctionType { ASTType.resultType = getASTType retType, ASTType.argumentTypes = map getASTType argTypes, ASTType.isVarArg = False } )

findLocalVarType :: [LocalVarType] -> Name -> S.Type
findLocalVarType localVars varName = case find (\(n, _) -> n == varName) localVars of
  Just (_, t) -> t
  Nothing -> error $ "Cannot determine type of variable " ++ show varName

structType :: [AST.Type] -> AST.Type
structType = ASTType.StructureType False

findTypeAlias :: Name -> SnocList Definition -> Definition
findTypeAlias name (SnocList []) = error $ "Type alias " ++ show name ++ " not found"
findTypeAlias name (SnocList (TypeDefinition n t : xs)) = if n == name then TypeDefinition n t else findTypeAlias name (SnocList xs)
findTypeAlias name (SnocList (_ : xs)) = findTypeAlias name (SnocList xs)

