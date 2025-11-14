module Parser where

import AST
import Lexer

parse :: [Token] -> (Expr, [Token])
parse (TokLet : TokIdent name : TokAssign : rest) =
  let (expr, rest') = parse rest
  in (Assign name expr, rest')

parse (TokNumber n : rest) = (Number n, rest)
parse (TokString s : rest) = (StringLit s, rest)
parse (TokIdent name : rest) = (Var name, rest)
parse (TokLParen : rest) =
  let (expr, rest') = parse rest
  in case rest' of
       TokRParen : rest'' -> (expr, rest'')
       _ -> error "Missing )"

parse (TokPlus : rest)  = parse rest
parse (TokMinus : rest) = parse rest
parse (TokStar : rest)  = parse rest
parse (TokSlash : rest) = parse rest

parse toks = (Seq [], toks)
