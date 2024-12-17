{-# LANGUAGE DerivingStrategies #-}

module Syntax where

import LLVM.AST.Name (Name)
import LLVM.IRBuilder.Module (ParameterName)

data TopLevel
  = Expr Expr
  | Declaration Declaration
  deriving stock (Eq, Ord, Show)

data Expr
  = Int Integer
  | Float Double
  | Bool Bool
  | TupleI Expr Expr
  | List [Expr]
  | Let Type Name Expr Expr
  | Var Name
  | Call Name [Expr]
  | If Expr Expr Expr
  | UnaryOp Name Expr
  | BinOp Name Expr Expr
  | FunOp Name
  deriving stock (Eq, Ord, Show)

data Declaration
  = Function Name [(Type, ParameterName)] Type Expr
  | Extern Name [(Type, ParameterName)] Type
  | Constant Type Name Expr
  deriving stock (Eq, Ord, Show)

data Type
  = Double
  | Integer
  | Boolean
  | Tuple Type Type
  | ListType Type
  | FunType [Type] Type
  deriving stock (Eq, Ord, Show)