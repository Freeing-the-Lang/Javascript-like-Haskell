module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit)

data Token
  = TokLet
  | TokIdent String
  | TokNumber Int
  | TokString String
  | TokAssign
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokLParen
  | TokRParen
  | TokSemi
  | TokEOF
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TokEOF]
tokenize (c:cs)
  | isAlpha c =
      let (name, rest) = span isAlphaNum (c:cs)
      in case name of
           "let" -> TokLet : tokenize rest
           _     -> TokIdent name : tokenize rest

  | isDigit c =
      let (digits, rest) = span isDigit (c:cs)
      in TokNumber (read digits) : tokenize rest

  | c == '"' =
      let (str, rest) = span (/= '"') cs
      in TokString str : tokenize (drop 1 rest)

  | c == '=' = TokAssign : tokenize cs
  | c == '+' = TokPlus   : tokenize cs
  | c == '-' = TokMinus  : tokenize cs
  | c == '*' = TokStar   : tokenize cs
  | c == '/' = TokSlash  : tokenize cs
  | c == '(' = TokLParen : tokenize cs
  | c == ')' = TokRParen : tokenize cs
  | c == ';' = TokSemi   : tokenize cs

  | otherwise = tokenize cs
