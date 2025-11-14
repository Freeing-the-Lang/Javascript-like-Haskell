module AST where

data Expr
  = Number Int
  | StringLit String
  | Var String
  | Assign String Expr
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Seq [Expr]
  deriving (Show, Eq)
