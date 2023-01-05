module Lexer (lex,Located) where

import Prelude hiding (lex)

import Token (Token)

data Located a

lex :: String -> [Located Token]
lex = undefined
