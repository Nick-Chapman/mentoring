module Token (Token(..)) where

data Token
  = UNEXPECTED Char
  | IDENT String
  | NUMBER Int
  | EQ
  | PLUS
  deriving Show
