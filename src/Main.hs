module Main where

import System.IO
import Lexer
import Parser
import AST
import Eval
import qualified Data.Map as M

main :: IO ()
main = do
    src <- readFile "examples/demo.jsh"

    putStrLn "[Lexer] Tokens:"
    let tokens = tokenize src
    print tokens

    putStrLn "\n[Parser] AST:"
    let (ast, _) = parse tokens
    print ast

    putStrLn "\n[Eval] Running program:"
    let (env, result) = eval M.empty ast
    putStrLn ("Result: " ++ show result)
