module Main where

import Lexer
import Parser
import Eval
import Control.Monad.State
import qualified Data.Map as M

main :: IO ()
main = do
  src <- readFile "examples/demo.jsh"
  let tokens = lexJS src
  print "[Tokens]"
  print tokens

  let ast = evalState parseProgram tokens
  print "[AST]"
  print ast

  _ <- foldl (\m e -> m >>= \env -> evalExpr env e) (return M.empty) ast
  return ()
