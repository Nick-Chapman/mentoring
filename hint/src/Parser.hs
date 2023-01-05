module Parser (parse,ErrorMsg) where

import Prelude hiding (lex,EQ)
import Ast (Program,Exp)
import qualified Ast (Program(..),Exp(..))
import Token (Token(..))
import Lexer (lex,Located(..))

data ErrorMsg = EM String deriving Show

parse :: String -> Either ErrorMsg Program
parse s = do
  let xs = lex s
  case parProgram xs of
    Right (p,[]) -> Right p
    Right (_,xs) -> Left (makeErrorMsg "trailing tokens at end of parse" xs)
    Left em -> Left em

parProgram :: Parser Program
parProgram = \case
  L{item=IDENT "main"}:xs ->
    case expect EQ xs of
      Left em -> Left em
      Right ((),xs) ->
        case (parExp xs) of
          Right (main,xs) -> Right (Ast.Program { main },xs)
          Left em -> Left em
  xs ->
    err "program" xs

parExp :: Parser Exp
parExp = \case
  L{item=NUMBER n}:xs ->
    parExpContinue (Ast.Lit n) xs
  L{item=LP}:xs -> do
    case parExp xs of
      Left em -> Left em
      Right (body,xs) ->
        case expect RP xs of
          Left em -> Left em
          Right ((),xs) -> parExpContinue body xs
  xs ->
    err "expression" xs

expect :: Token -> Parser ()
expect sought = \case
  L{item=got}:xs | (got == sought)->
    Right ((),xs)
  xs ->
    err (show sought) xs

parExpContinue :: Exp -> Parser Exp
parExpContinue e1 = \case
  L{item=PLUS}:xs -> do
    case parExp xs of
      Left em -> Left em
      Right (e2,xs) ->
        Right (Ast.Add e1 e2,xs)
  L{item=DASH}:xs -> do
    case parExp xs of
      Left em -> Left em
      Right (e2,xs) ->
        Right (Ast.Sub e1 e2,xs)
  xs ->
    Right (e1,xs)


type Parser a = [Located Token] -> Either ErrorMsg (a,[Located Token])

err :: String -> Parser a
err sought xs = do Left (makeErrorMsg ("looking for '" ++ sought ++ "' -- but got") xs)

makeErrorMsg :: String -> [Located Token] -> ErrorMsg
makeErrorMsg mes lts = do
  EM (mes ++ ": " ++
       case lts of
         [] -> "no more tokens"
         lt1:_ -> "unexpected: " ++ show lt1
     )
