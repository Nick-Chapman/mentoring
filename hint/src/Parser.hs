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
        case (parExp xs) of
          (main,[]) -> Ast.Program { main }
          (_,xs) -> err xs "program after main expression"
      xs ->
        err xs "program after main"

  xs ->
    err xs "program!"


parExp :: [Located Token] -> (Exp,[Located Token])
parExp = \case
  L{item=NUMBER n}:xs ->
    parExpContinue (Ast.Lit n) xs

  L{item=LP}:xs -> do
    case parExp xs of
      (body,xs) ->
        case xs of
          L{item=RP}:xs -> do
            parExpContinue body xs
          _ -> do
            err xs "exp expected RP"

  xs ->
    err xs "exp!"


parExpContinue :: Exp -> [Located Token] -> (Exp,[Located Token])
parExpContinue e1 = \case

  L{item=PLUS}:xs -> do
    case parExp xs of
      (e2,xs) ->
        (Ast.Add e1 e2,xs)

  L{item=DASH}:xs -> do
    case parExp xs of
      (e2,xs) ->
        (Ast.Sub e1 e2,xs)

  xs ->
    (e1,xs)
