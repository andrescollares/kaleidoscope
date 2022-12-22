module Syntax where

type Name = String

data Expr
  = Float Double
  | BinOp Op Expr Expr
  | Var String
  | Call Name [Expr]
  | Function Name [Name] Expr
  | Extern Name [Name]
  deriving (Eq, Ord, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  deriving (Eq, Ord, Show)
