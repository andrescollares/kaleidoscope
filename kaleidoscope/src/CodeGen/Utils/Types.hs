{-# LANGUAGE OverloadedStrings #-}

module CodeGen.Utils.Types where

import Data.String (fromString)
import Data.List (find)
import LLVM.AST as AST ( Name (Name), Type (PointerType, NamedTypeReference, IntegerType, FloatingPointType, VoidType, FunctionType, pointerReferent, pointerAddrSpace, StructureType), Operand (..) )
import qualified LLVM.AST.Type as ASTType
import qualified Syntax as S
import LLVM.AST.AddrSpace (AddrSpace(AddrSpace))
import LLVM.AST.Type (ptr)
import LLVM.AST.Constant as AST.Constant (Constant(..))


type LocalVarType = (Name, S.Type)

getExpressionType :: S.Operand -> [LocalVarType] -> AST.Type
getExpressionType (S.Int _) _ = ASTType.i32
getExpressionType (S.Float _) _ = ASTType.double
getExpressionType (S.Bool _) _ = ASTType.i1
getExpressionType (S.TupleI e1 e2) localVars = ASTType.StructureType False [getExpressionType e1 localVars, getExpressionType e2 localVars]
getExpressionType (S.Call functionName _) localVars = getASTType $ findLocalVarType localVars functionName
getExpressionType (S.Var varName) localVars = getASTType $ findLocalVarType localVars varName
getExpressionType (S.UnaryOp unOp (S.TupleI e1 e2)) localVars =
  case unOp of
    "fst" -> getExpressionType e1 localVars
    "snd" -> getExpressionType e2 localVars
    _ -> error "Unsupported unary operation on Tuple"
getExpressionType (S.UnaryOp unOp (S.List (x:_))) localVars =
  case unOp of
    "head" -> getExpressionType x localVars
    "tail" -> listPointerType x localVars
    _ -> error "Unsupported unary operation on List"
getExpressionType (S.UnaryOp unOp e) localVars =
  case unOp of
    "!" -> ASTType.i1
    "-" -> getExpressionType e localVars
    _ -> getExpressionType e localVars

-- TODO: this is potentially O(2^n)!!!
getExpressionType (S.BinOp name a b) localVars
  | name == ":" = typeOfB
  | name `elem` ["==", "!=", "<", ">", "<=", ">="] = ASTType.i1
  | typeOfA == ASTType.double || typeOfB == ASTType.double = ASTType.double
  | otherwise = typeOfA
  where
      typeOfA = getExpressionType a localVars
      typeOfB = getExpressionType b localVars
getExpressionType (S.Let varType varName _ e) localVars = getExpressionType e $ localVars ++ [(varName, varType)]
getExpressionType (S.If _ e1 e2) localVars =
  if e1Type == getExpressionType e2 localVars
    then e1Type
    else error "Types of both sides of if statement should be the same"
  where
    e1Type = getExpressionType e1 localVars
-- getExpressionType (List [x]) localVars = getExpressionType x localVars
getExpressionType (S.List (x:_)) localVars = listPointerType x localVars
-- FIXME: (?) empty list defaults to int list
getExpressionType (S.List []) _ = PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
getExpressionType (S.FunOp _) _ = PointerType VoidType (AddrSpace 0)
-- getExpressionType e localVars = error $ "Unsupported expression: " ++ show e ++ "Local vars: " ++ show localVars

findLocalVarType :: [LocalVarType] -> Name -> S.Type
findLocalVarType localVars varName = case find (\(n, _) -> n == varName) localVars of
  Just (_, t) -> t
  Nothing -> error $ "Cannot determine type of variable " ++ show varName

listPointerType :: S.Operand -> [LocalVarType] -> AST.Type
listPointerType listElement localVars = case getExpressionType listElement localVars of -- TODO: local vars param
  IntegerType { ASTType.typeBits = 32 } -> PointerType (NamedTypeReference (Name (fromString "IntList"))) (AddrSpace 0)
  IntegerType { ASTType.typeBits = 1 } -> PointerType (NamedTypeReference (Name (fromString "BoolList"))) (AddrSpace 0)
  FloatingPointType _ -> PointerType (NamedTypeReference (Name (fromString "FloatList"))) (AddrSpace 0)
  _ -> error "Unsupported list element type"

getASTType :: S.Type -> AST.Type
getASTType S.Double = ASTType.double
getASTType S.Integer = ASTType.i32
getASTType S.Boolean = ASTType.i1
getASTType (S.Tuple t1 t2) = ASTType.StructureType False [getASTType t1, getASTType t2]
getASTType (S.ListType t) = PointerType (NamedTypeReference (Name (fromString $ listSyntaxPointerTypeName t))) (AddrSpace 0)
getASTType (S.FunType argTypes retType) = ptr ( FunctionType { ASTType.resultType = getASTType retType, ASTType.argumentTypes = map getASTType argTypes, ASTType.isVarArg = False } )

listSyntaxPointerTypeName :: S.Type -> String
listSyntaxPointerTypeName S.Integer = "IntList"
listSyntaxPointerTypeName S.Boolean = "BoolList"
listSyntaxPointerTypeName S.Double = "FloatList"
listSyntaxPointerTypeName x = error "Unsupported list element type: " <> show x

-- findTypeAlias :: Name -> SnocList Definition -> Definition
-- findTypeAlias name (SnocList []) = error $ "Type alias " ++ show name ++ " not found"
-- findTypeAlias name (SnocList (TypeDefinition n t : xs)) = if n == name then TypeDefinition n t else findTypeAlias name (SnocList xs)
-- findTypeAlias name (SnocList (_ : xs)) = findTypeAlias name (SnocList xs)

operandType :: AST.Operand -> ASTType.Type
operandType op = case op of
  LocalReference ty _ -> ty
  ConstantOperand con -> case con of
    AST.Constant.Int _ _ -> ASTType.i32
    AST.Constant.Float _ -> ASTType.double
    Struct { memberValues = [a, b] } -> StructureType False [operandType $ ConstantOperand a, operandType $ ConstantOperand b]
    -- TODO: empty list defaults to int list
    Null _ -> ASTType.PointerType {pointerReferent = NamedTypeReference (Name $ fromString "IntList"), pointerAddrSpace = AddrSpace 0}
    _ -> error $ "Unsupported constant operand type: " ++ show op
  _ -> error $ "Unsupported operand type: " ++ show op