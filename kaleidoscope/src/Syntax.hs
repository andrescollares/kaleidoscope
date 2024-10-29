{-# LANGUAGE DerivingStrategies #-}

module Syntax where

import LLVM.AST.Name (Name)
import LLVM.IRBuilder.Module (ParameterName)

-- TODO: change name to TopLevel
data Expr
  = Operand Operand
  | TopLevel Declaration
  deriving stock (Eq, Ord, Show)

-- TODO: change name to Expr
data Operand
  = Int Integer
  | Float Double
  | Bool Bool
  | TupleI Operand Operand
  | List [Operand]
  | Let Type Name Operand Operand
  | Var Name
  | Call Name [Operand]
  | If Operand Operand Operand
  | UnaryOp Name Operand
  | BinOp Name Operand Operand
  | FunOp Name
  deriving stock (Eq, Ord, Show)

data Declaration
  = Function Name [(Type, ParameterName)] Type Operand
  -- Internal function
  | Extern Name [(Type, ParameterName)] Type
  | Constant Type Name Operand
  | TypeDef Name Type -- TODO: BUG: type defs are optimized out if no instances are found. Even with optLevel 0
  deriving stock (Eq, Ord, Show)

data Type
  = Double
  | Integer
  | Boolean
  | Tuple Type Type
  | ListType Type
  | FunType [Type] Type
  deriving stock (Eq, Ord, Show)