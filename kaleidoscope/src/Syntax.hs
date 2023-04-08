{-# LANGUAGE DerivingStrategies #-}

module Syntax where

type Name = String

data Expr
  = Float Double
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  | BinOp Name Expr Expr
  | UnaryOp Name Expr
  deriving stock (Eq, Ord, Show)
