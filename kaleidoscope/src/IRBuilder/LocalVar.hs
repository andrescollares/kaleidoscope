{-# LANGUAGE DerivingStrategies #-}

{-# LANGUAGE OverloadedStrings #-}


module IRBuilder.LocalVar where

-- import Data.Maybe

import Data.ByteString.Short ( ShortByteString )
import qualified Data.List as DL
import Data.String ( IsString(fromString) )
import qualified Data.Text as T
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Global as G (Global (name, type'), returnType)
import LLVM.AST.Type (ptr)
import qualified LLVM.AST.Type as ASTType
import LLVM.IRBuilder.Internal.SnocList ( SnocList(SnocList) )
import LLVM.IRBuilder.Module (ParameterName (ParameterName))
import Syntax as S
import Types

type LocalVar = (Maybe ShortByteString, AST.Operand) -- alias, value

definitionsToLocalVars :: SnocList Definition -> [LocalVarType]
definitionsToLocalVars (SnocList defs) =
  map
    ( \def -> case def of
        GlobalDefinition AST.GlobalVariable {G.name = n, G.type' = t} -> (n, llvmTypeToSyntaxType t)
        GlobalDefinition AST.Function {G.name = n, G.returnType = retT} -> (n, llvmTypeToSyntaxType retT)
        TypeDefinition n (Just t) -> (n, llvmTypeToSyntaxType t)
        _ -> error $ "Unsupported definition " ++ show def
    )
    defs

-- TODO: probably missing some cases

llvmTypeToSyntaxType :: ASTType.Type -> S.Type
llvmTypeToSyntaxType t = case t of
  ASTType.FloatingPointType {floatingPointType = DoubleFP} -> Double
  ASTType.IntegerType {typeBits = 32} -> Integer
  ASTType.IntegerType {typeBits = 1} -> Boolean
  ASTType.StructureType { elementTypes = [t1, t2] } -> Tuple (llvmTypeToSyntaxType t1) (llvmTypeToSyntaxType t2)
  ASTType.PointerType { pointerReferent = ASTType.NamedTypeReference (Name n) } -> case n of
    "IntList" -> ListType Integer
    "BoolList" -> ListType Boolean
    "FloatList" -> ListType Double
    _ -> error $ "Unsupported list type " ++ show n
  _ -> error $ "Unsupported type " ++ show t

functionLocalVar :: [AST.Operand] -> [(S.Type, ParameterName)] -> Name -> AST.Type -> [LocalVar]
functionLocalVar operands functionParameters (Name n) t = localVarsFallback operands ++ [(Just n, getFunctionOperand (Name n) t (functionLocalVarParameters functionParameters, False))]
functionLocalVar _ _ _ _ = error "Function lacks a name."

localVarsFallback :: [AST.Operand] -> [LocalVar]
localVarsFallback = map (\operand -> (Nothing, operand))

functionLocalVarParameters :: [(S.Type, ParameterName)] -> [Parameter]
functionLocalVarParameters = map (\(t, ParameterName n) -> Parameter (getASTType t) (Name n) [])

getLocalVarName :: ShortByteString -> [LocalVar] -> Maybe LocalVar
getLocalVarName n = DL.find (`matchName` n)

matchName :: LocalVar -> ShortByteString -> Bool
-- TODO: More explicit matching
matchName (Just _, ConstantOperand (C.GlobalReference PointerType {pointerReferent = ASTType.FunctionType {}, pointerAddrSpace = _} (Name funName))) n = funName == n
-- matchName (Just _, puntero_anonimo_a_funcion) n = varName == n
matchName (Just varName, _) n = varName == n
matchName (Nothing, LocalReference _ (Name varName)) n = removeEnding varName == n
matchName (Nothing, LocalReference _ (UnName varNumber)) n = show varNumber == show n
matchName _ _ = False

-- TODO: Rework this function later, don't use show
-- bytestring > 11.smth has implemented this function but llvm 12 doesn't permit bytestring > 11
removeEnding :: ShortByteString -> ShortByteString
removeEnding variableName
  | T.isInfixOf "_" (T.pack $ show variableName) = fromString $ tail $ reverse $ tail $ dropWhile (/= '_') (reverse $ show variableName)
  | otherwise = variableName

-- TODO: lots of DRY to do here
-- The function shouldn't live in the local vars
getFunctionFromDefs :: SnocList Definition -> Name -> Maybe Definition
getFunctionFromDefs defs functionName = find (`matchNameGlobal` functionName) defs Nothing
  where
    matchNameGlobal :: Definition -> Name -> Bool
    matchNameGlobal (GlobalDefinition AST.Function {G.name = n}) nameToMatch = n == nameToMatch
    matchNameGlobal _ _ = False
    find :: (a -> Bool) -> SnocList a -> Maybe a -> Maybe a
    find p (SnocList (x : xs)) res
      | p x = Just x
      | otherwise = find p (SnocList xs) res
    find _ (SnocList []) res = res

getConstantFromDefs :: SnocList Definition -> Name -> Maybe Definition
getConstantFromDefs defs constantName = find (`matchNameGlobal` constantName) defs Nothing
  where
    matchNameGlobal :: Definition -> Name -> Bool
    matchNameGlobal (GlobalDefinition AST.GlobalVariable {G.name = n}) nameToMatch = n == nameToMatch
    matchNameGlobal _ _ = False
    find :: (a -> Bool) -> SnocList a -> Maybe a -> Maybe a
    find p (SnocList (x : xs)) res
      | p x = Just x
      | otherwise = find p (SnocList xs) res
    find _ (SnocList []) res = res

getFunctionOperand :: Name -> AST.Type -> ([Parameter], Bool) -> AST.Operand
getFunctionOperand fn retType (params, _) = ConstantOperand $ C.GlobalReference (ptr $ FunctionType retType (map (\(AST.Parameter t _ _) -> t) params) False) fn