
module Par (Parser,ErrorMsg,run,err,next,peek) where -- combinators

import Control.Monad (ap,liftM)
import Lexer (lex,Located(..))
import Prelude hiding (lex,EQ)
import Token (Token(..))

instance Functor Parser where fmap = liftM
instance Applicative Parser where pure = return; (<*>) = ap
instance Monad Parser where return = ret; (>>=) = bind

data Parser a = Parser ([Located Token] -> Either ErrorMsg (a,[Located Token]))

run :: Parser a -> String -> Either ErrorMsg a
run (Parser pf) s = do
  let xs = lex s
  case pf xs of
    Left em -> Left em
    Right (a,[]) -> Right a
    Right (_,xs) -> Left (makeErrorMsg "trailing tokens at end of parse" xs)

ret :: a -> Parser a
ret a = Parser $ \xs -> Right (a,xs)

next :: Parser (Maybe Token) -- consuming
next = Parser $ \case
  [] -> Right (Nothing,[])
  L{item=tok}:xs -> Right (Just tok,xs)

peek :: Parser (Maybe Token) -- non consuming
peek = Parser $ \case
  [] -> Right (Nothing,[])
  xs@(L{item=tok}:_) -> Right (Just tok,xs)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind (Parser pf) f = Parser $ \xs ->
  case pf xs of
    Left em -> Left em
    Right (a,xs) -> do
      case f a of
        Parser pf2 -> pf2 xs

err :: String -> Parser a
err s = Parser $ \xs -> Left (makeErrorMsg s xs)

data ErrorMsg = EM String

instance Show ErrorMsg where
  show (EM s) = "Error: " ++ s

makeErrorMsg :: String -> [Located Token] -> ErrorMsg
makeErrorMsg mes lts = do
  EM (mes ++ ": " ++
       case lts of
         [] -> "no more tokens"
         lt1:_ -> "unexpected: " ++ show lt1
     )
