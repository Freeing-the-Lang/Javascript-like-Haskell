module Lexer where

data Token
  = TokLet
  | TokIdent String
  | TokNumber Int
  | TokString String
  | TokEqual
  | TokPlus | TokMinus | TokStar | TokSlash
  | TokLParen | TokRParen
  | TokSemi
  | TokEOF
  deriving (Show, Eq)

lexJS :: String -> [Token]
lexJS [] = [TokEOF]
lexJS (' ' : xs) = lexJS xs
lexJS ('\n': xs) = lexJS xs
lexJS ('\t': xs) = lexJS xs

lexJS ('l':'e':'t':xs) = TokLet : lexJS xs

lexJS (c:cs)
  | isAlpha c = let (name, rest) = span isAlphaNum (c:cs)
                in TokIdent name : lexJS rest

  | isDigit c = let (number, rest) = span isDigit (c:cs)
                in TokNumber (read number) : lexJS rest

lexJS ('"' : xs) =
  let (s, rest) = span (/= '"') xs
  in TokString s : lexJS (tail rest)

lexJS ('=':xs) = TokEqual : lexJS xs
lexJS ('+':xs) = TokPlus : lexJS xs
lexJS ('-':xs) = TokMinus : lexJS xs
lexJS ('*':xs) = TokStar : lexJS xs
lexJS ('/':xs) = TokSlash : lexJS xs
lexJS ('(':xs) = TokLParen : lexJS xs
lexJS (')':xs) = TokRParen : lexJS xs
lexJS (';':xs) = TokSemi   : lexJS xs

lexJS (_:xs) = lexJS xs
