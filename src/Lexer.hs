module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit)

data Token
  = TokNumber Int
  | TokIdent String
  | TokPlus
  | TokMinus
  | TokStar
  | TokSlash
  | TokEOF
  deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TokEOF]
tokenize (c:cs)
  | isAlpha c =
      let (name, rest) = span isAlphaNum (c:cs)
      in TokIdent name : tokenize rest      

  | isDigit c =
      let (digits, rest) = span isDigit (c:cs)
      in TokNumber (read digits) : tokenize rest

  | c == '+' = TokPlus  : tokenize cs
  | c == '-' = TokMinus : tokenize cs
  | c == '*' = TokStar  : tokenize cs
  | c == '/' = TokSlash : tokenize cs

  | otherwise = tokenize cs
