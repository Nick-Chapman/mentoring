module Parser (parse,ErrorMsg) where

import Prelude hiding (lex)
import Ast (Program(..),Exp(..))
import Lexer (lex)

type ErrorMsg = String

parse :: String -> Either ErrorMsg Program
parse _ = do
  let _ = undefined lex
  let p = Program { main = Lit 42 }
  Right p
