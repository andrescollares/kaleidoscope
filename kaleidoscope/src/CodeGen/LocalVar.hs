{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module CodeGen.LocalVar where

import Data.ByteString.Short (ShortByteString)
import qualified Data.List as List
import Data.String (IsString (fromString))
import qualified Data.Text as T
import LLVM.AST as AST hiding (function)
import qualified LLVM.AST.Constant as C
import LLVM.AST.Type (ptr)
import qualified LLVM.AST.Type as ASTType

type LocalVar = (Maybe ShortByteString, AST.Operand) -- alias, value

localVarsFallback :: [AST.Operand] -> [LocalVar]
localVarsFallback = map (\operand -> (Nothing, operand))

getLocalVarByName :: ShortByteString -> [LocalVar] -> Maybe LocalVar
getLocalVarByName name = List.find (`matchName` name)
  where
    matchName :: LocalVar -> ShortByteString -> Bool
    matchName (Just _, ConstantOperand (C.GlobalReference PointerType {pointerReferent = ASTType.FunctionType {}, pointerAddrSpace = _} (Name funName))) n = funName == n
    matchName (Just varName, _) n = varName == n
    matchName (Nothing, LocalReference _ (Name varName)) n = removeEnding varName == n
    matchName (Nothing, LocalReference _ (UnName varNumber)) n = show varNumber == show n
    matchName _ _ = False
    -- TODO: Rework this function later, don't use show
    -- bytestring > 11.smth has implemented this function but llvm-hs 12 doesn't permit bytestring > 11
    removeEnding :: ShortByteString -> ShortByteString
    removeEnding variableName
      | T.isInfixOf "_" (T.pack $ show variableName) = fromString $ tail $ reverse $ tail $ dropWhile (/= '_') (reverse $ show variableName)
      | otherwise = variableName

getFunctionOperand :: Name -> AST.Type -> ([Parameter], Bool) -> AST.Operand
getFunctionOperand fn retType (params, _) = ConstantOperand $ C.GlobalReference (ptr $ FunctionType retType (map (\(AST.Parameter t _ _) -> t) params) False) fn