{-# LANGUAGE DerivingStrategies #-}

module Syntax where
import LLVM.IRBuilder.Module ( ParameterName )
import LLVM.AST.Name ( Name )
import Data.ByteString.Short (ShortByteString)


data Expr
  = Int Integer
  | Float Double
  | Bool Bool
  | Let Type Name Expr Expr
  | Var Name
  | Constant Type Name Expr
  | Call Name [Expr]
  | If Expr Expr Expr
  | Function Name [(Type, ParameterName)] Type Expr
  | Extern Name [(Type, ParameterName)] Type
  | UnaryOp ShortByteString Expr
  | BinOp ShortByteString Expr Expr
  | UnaryDef ShortByteString [ParameterName] Expr -- TODO: adapt to new syntax
  | BinaryDef ShortByteString [ParameterName] Expr -- TODO: adapt to new syntax
  deriving stock (Eq, Ord, Show)

data Type 
  = Double
  | Integer
  | Boolean
  deriving stock (Eq, Ord, Show)

-- | Enum Literal
-- | Struct [(Name, Literal)]