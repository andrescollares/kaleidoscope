{-# LANGUAGE DerivingStrategies #-}

module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | Constant Name Expr
  | Function Name [Name] Expr
  | Extern Name [Name]
  | UnaryOp Name Expr
  | BinOp Name Expr Expr
  | UnaryDef Name [Name] Expr
  | BinaryDef Name [Name] Expr
  | If Expr Expr Expr
  | Let Name Expr Expr
  | For Name Expr Expr Expr Expr
  deriving stock (Eq, Ord, Show)
