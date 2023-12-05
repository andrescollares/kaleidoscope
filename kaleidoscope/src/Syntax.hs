{-# LANGUAGE DerivingStrategies #-}

module Syntax where
import Data.ByteString.Short (ShortByteString)
import LLVM.IRBuilder.Module
import LLVM.AST.Name


data Expr
  = Float Double
  | Let Name Expr Expr
  | Var Name
  | Call Name [Expr]
  | Function Name [ParameterName] Expr
  | Extern Name [Name]
  | Constant Name Double
  | UnaryOp ShortByteString Expr
  | BinOp ShortByteString Expr Expr
  | UnaryDef ShortByteString [ParameterName] Expr -- TODO: adapt to new syntax
  | BinaryDef ShortByteString [ParameterName] Expr -- TODO: adapt to new syntax
  | If Expr Expr Expr
  deriving stock (Eq, Ord, Show)
