module Eval where

import AST
import qualified Data.Map as M

type Env = M.Map String Int

evalExpr :: Env -> Expr -> IO Env
evalExpr env (assign name val) = do
  v <- evalVal env val
  let env' = M.insert name v env
  return env'

evalExpr env e = do
  _ <- evalVal env e
  return env

evalVal :: Env -> Expr -> IO Int
evalVal env (number n) = return n
evalVal env (var name) = return (env M.! name)
evalVal env (binop "+" l r) = (+) <$> evalVal env l <*> evalVal env r
evalVal env (binop "-" l r) = (-) <$> evalVal env l <*> evalVal env r

evalVal env (call "print" [x]) = do
  v <- evalVal env x
  print v
  return v

evalVal _ other = error ("Unknown expression: " ++ show other)
