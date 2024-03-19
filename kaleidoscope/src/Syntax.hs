{-# LANGUAGE DerivingStrategies #-}

module Syntax where

import Data.ByteString.Short (ShortByteString)
import LLVM.AST.Name (Name)
import LLVM.IRBuilder.Module (ParameterName)

data Expr
  = Operand Operand
  | TopLevel Declaration
  deriving stock (Eq, Ord, Show)

data Operand
  = Int Integer
  | Float Double
  | Bool Bool
  | Let Type Name Operand Operand
  | Var Name
  | Call Name [Operand]
  | If Operand Operand Operand
  | UnaryOp ShortByteString Operand
  | BinOp ShortByteString Operand Operand
  | UnaryDef ShortByteString [ParameterName] Operand -- TODO: adapt to new syntax
  | BinaryDef ShortByteString [ParameterName] Operand -- TODO: adapt to new syntax
  deriving stock (Eq, Ord, Show)

data Declaration
  = Function Name [(Type, ParameterName)] Type Operand
  | Extern Name [(Type, ParameterName)] Type
  | Constant Type Name Operand
  deriving stock (Eq, Ord, Show)

data Type
  = Double
  | Integer
  | Boolean
  deriving stock (Eq, Ord, Show)

-- | Enum Literal
-- | Struct [(Name, Literal)]