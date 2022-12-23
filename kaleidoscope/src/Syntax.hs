-- module Syntax where

-- type Name = String

-- data Expr
--   = Float Double
--   | BinOp Op Expr Expr
--   | Var String
--   | Call Name [Expr]
--   | Function Name [Name] Expr
--   | Extern Name [Name]
--   deriving (Eq, Ord, Show)


-- -- Op not used in chapter 3...
-- data Op
--   = Plus
--   | Minus
--   | Times
--   | Divide
--   deriving (Eq, Ord, Show)

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
  deriving (Eq, Ord, Show)
