module Parser (parse,ErrorMsg) where

import Prelude hiding (lex,EQ)
import Ast (Program,Exp)
import qualified Ast (Program(..),Exp(..))
import Token (Token(..))
import Lexer (lex,Located(..))

type ErrorMsg = String

parse :: String -> Either ErrorMsg Program
parse s = do
  let xs = lex s
  Right (parProgram xs)

err :: [Located Token] -> String -> a
err lts mes = do
  error ("parse error: " ++ mes ++ " -- " ++
         case lts of
           [] -> "no more tokens"
           lt1:_ -> "unexpected: " ++ show lt1
        )

parProgram :: [Located Token] -> Program
parProgram = \case
  L{item=IDENT "main"}:xs ->
    case xs of
      L{item=EQ}:xs -> do
        Ast.Program { main = parExp xs }
      xs ->
        err xs "program after main"

  xs ->
    err xs "program!"

parExp :: [Located Token] -> Exp
parExp = \case
  L{item=NUMBER n}:xs ->
    case xs of
      [] -> Ast.Lit n
      L{item=PLUS}:xs -> do
        let e1 = Ast.Lit n
        let e2 = parExp xs
        Ast.Add e1 e2
      xs -> err xs "exp after number"
  xs -> err xs "exp!"
