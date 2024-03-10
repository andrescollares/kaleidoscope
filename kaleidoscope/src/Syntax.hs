{-# LANGUAGE DerivingStrategies #-}

module Syntax where
import LLVM.IRBuilder.Module ( ParameterName )
import LLVM.AST.Name ( Name )
import Data.ByteString.Short (ShortByteString)

data Expr
  = Operand SyntaxOperand
  | TopLevel SyntaxDef
  deriving stock (Eq, Ord, Show)

data SyntaxOperand
  = Int Integer
  | Float Double
  | Bool Bool
  | Let Type Name SyntaxOperand SyntaxOperand
  | Var Name
  | Constant Type Name SyntaxOperand
  | Call Name [SyntaxOperand]
  | If SyntaxOperand SyntaxOperand SyntaxOperand
  -- | Function Name [(Type, ParameterName)] Type Expr
  -- | Extern Name [(Type, ParameterName)] Type
  | UnaryOp ShortByteString SyntaxOperand
  | BinOp ShortByteString SyntaxOperand SyntaxOperand
  | UnaryDef ShortByteString [ParameterName] SyntaxOperand -- TODO: adapt to new syntax
  | BinaryDef ShortByteString [ParameterName] SyntaxOperand -- TODO: adapt to new syntax
  deriving stock (Eq, Ord, Show)

data SyntaxDef
  = Function Name [(Type, ParameterName)] Type SyntaxOperand
  | Extern Name [(Type, ParameterName)] Type

  deriving stock (Eq, Ord, Show)

data Type 
  = Double
  | Integer
  | Boolean
  deriving stock (Eq, Ord, Show)

-- | Enum Literal
-- | Struct [(Name, Literal)]