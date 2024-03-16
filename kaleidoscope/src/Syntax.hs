{-# LANGUAGE DerivingStrategies #-}

module Syntax where
import LLVM.IRBuilder.Module ( ParameterName )
import LLVM.AST.Name ( Name )
import Data.ByteString.Short (ShortByteString)

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
  | Constant Type Name Operand
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

  deriving stock (Eq, Ord, Show)

data Type 
  = Double
  | Integer
  | Boolean
  deriving stock (Eq, Ord, Show)

-- | Enum Literal
-- | Struct [(Name, Literal)]