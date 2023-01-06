module Parser (parse,ErrorMsg) where

import Ast (Program,Exp)
import Par (Parser,ErrorMsg,next,peek,err)
import Prelude hiding (lex,EQ)
import Token (Token(..))
import qualified Ast (Program(..),Exp(..))
import qualified Par (run)

parse :: String -> Either ErrorMsg Program
parse = Par.run program

program :: Parser Program
program = do
  expect (IDENT "main")
  expect EQ
  main <- expresssion
  pure Ast.Program { main }

expresssion :: Parser Exp
expresssion = next >>= \case
  Just (NUMBER n) -> do
    expresssionContinue (Ast.Lit n)
  Just LP -> do
    body <- expresssion
    expect RP
    expresssionContinue body
  _ ->
    err "expression"

expresssionContinue :: Exp -> Parser Exp
expresssionContinue e1 = peek >>= \case
  Just PLUS -> do
    consume
    e2 <- expresssion
    pure (Ast.Add e1 e2)
  Just DASH -> do
    consume
    e2 <- expresssion
    pure (Ast.Sub e1 e2)
  _ -> do
    pure e1

consume :: Parser ()
consume = do
  _ <- next
  pure ()

expect :: Token -> Parser ()
expect sought = peek >>= \case
  Just got | sought == got -> do consume; pure ()
  _ -> err ("sought: " ++ show sought)
