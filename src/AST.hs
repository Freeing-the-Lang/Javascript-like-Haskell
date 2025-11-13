module AST where

data Expr
  = number    Int
  | str       String
  | var       String
  | assign    String Expr
  | binop     String Expr Expr
  | call      String [Expr]
  deriving (Show, Eq)
