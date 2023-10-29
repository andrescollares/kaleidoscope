{-# LANGUAGE DerivingStrategies #-}

module Syntax where
import Data.ByteString.Short (ShortByteString)
import LLVM.IRBuilder.Module
import LLVM.AST.Name

-- type Name = String

-- data Expr
--   = Float Double
--   | Var String
--   | Call Name [Expr]
--   | Constant Name Expr
--   | Function Name [Name] Expr
--   | Extern Name [Name]
--   | UnaryOp Name Expr
--   | BinOp Name Expr Expr
--   | UnaryDef Name [Name] Expr
--   | BinaryDef Name [Name] Expr
--   | If Expr Expr Expr
--   | Let Name Expr Expr
--   | For Name Expr Expr Expr Expr
--   deriving stock (Eq, Ord, Show)


data Expr
  = Float Double
  | Let Name Expr Expr
  | Var Name
  | Call Name [Expr]
  | Function Name [ParameterName] Expr
  | Extern Name [Name]
  | UnaryOp ShortByteString Expr
  | BinOp ShortByteString Expr Expr
  | UnaryDef ShortByteString [Name] Expr -- TODO: adapt to new syntax
  | BinaryDef ShortByteString [Name] Expr -- TODO: adapt to new syntax
  | If Expr Expr Expr
  deriving stock (Eq, Ord, Show)
