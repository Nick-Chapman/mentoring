module Parser (parse,ErrorMsg) where

import Prelude hiding (lex)
import Ast (Program)
import Lexer (lex)

type ErrorMsg = String

parse :: String -> Either ErrorMsg Program
parse = undefined lex
