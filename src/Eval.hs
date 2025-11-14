module Eval where

import AST
import qualified Data.Map as M

type Env = M.Map String Expr

eval :: Env -> Expr -> (Env, Expr)
eval env (Number n) = (env, Number n)
eval env (StringLit s) = (env, StringLit s)

eval env (Var x) =
  case M.lookup x env of
    Just v  -> (env, v)
    Nothing -> error ("Undefined variable: " ++ x)

eval env (Assign name val) =
  let (env', v) = eval env val
      env'' = M.insert name v env'
  in (env'', v)

eval env (Add a b) =
  let (_, Number x) = eval env a
      (_, Number y) = eval env b
  in (env, Number (x + y))

eval env (Seq xs) =
  foldl step (env, Number 0) xs
  where
    step (e, _) expr = eval e expr
