module Parser where

import Lexer
import AST
import Control.Monad.State

type Parser a = State [Token] a

peek :: Parser Token
peek = gets head

consume :: Parser Token
consume = do
  (x:xs) <- get
  put xs
  return x

parseProgram :: Parser [Expr]
parseProgram = do
  tk <- peek
  case tk of
    TokEOF -> return []
    _      -> do
      s <- parseStmt
      rest <- parseProgram
      return (s : rest)

parseStmt :: Parser Expr
parseStmt = do
  tk <- peek
  case tk of
    TokLet -> do
      consume
      TokIdent name <- consume
      consume -- '='
      value <- parseExpr
      optionalSemi
      return (assign name value)
    _ -> do
      e <- parseExpr
      optionalSemi
      return e

optionalSemi = do
  tk <- peek
  case tk of
    TokSemi -> consume >> return ()
    _       -> return ()

parseExpr :: Parser Expr
parseExpr = do
  left <- parsePrimary
  parseBinOp left

parseBinOp left = do
  tk <- peek
  case tk of
    TokPlus  -> consume >> parseExpr >>= \r -> return (binop "+" left r)
    TokMinus -> consume >> parseExpr >>= \r -> return (binop "-" left r)
    _ -> return left

parsePrimary :: Parser Expr
parsePrimary = do
  tk <- consume
  case tk of
    TokNumber n -> return (number n)
    TokString s -> return (str s)
    TokIdent name -> do
      next <- peek
      case next of
        TokLParen -> do
          consume
          args <- parseArgs
          consume
          return (call name args)
        _ -> return (var name)
    _ -> error ("Unexpected token: " ++ show tk)

parseArgs = do
  tk <- peek
  case tk of
    TokRParen -> return []
    _ -> do
      e <- parseExpr
      restArgs e

restArgs x = do
  tk <- peek
  case tk of
    TokRParen -> return [x]
    _ -> do
      e <- parseExpr
      xs <- restArgs e
      return (x:xs)
