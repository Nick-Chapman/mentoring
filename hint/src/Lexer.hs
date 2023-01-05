module Lexer (lex,Located(..)) where

import Data.Char (isDigit)
import Prelude hiding (lex,EQ)

import Token (Token(..))

data Located a = L { start :: Pos, end :: Pos, item :: a }

instance Show a => Show (Located a) where
  show L{start,end,item} = show item ++ " at " ++ show start ++ "-" ++ show end

data Pos = Pos { line :: Int, col :: Int }

instance Show Pos where
  show Pos {line,col} = show line ++ ":" ++ show col

step :: Int -> Pos -> Pos
step n Pos{line,col} = Pos { line, col = col + n }

lex :: String -> [Located Token]
lex s = loop Pos { line = 1, col = 0 } s


loop :: Pos -> String -> [Located Token]
loop start = \case

  'm':'a':'i':'n':xs -> do
    let end = step (length "main" - 1) start
    L { start, end, item = IDENT "main" } : loop (step 1 end) xs

  ' ':xs -> do
    loop start xs

  '=':xs -> do
    let end = start
    L { start, end = start, item = EQ } : loop (step 1 end) xs

  '+':xs -> do
    let end = start
    L { start, end = start, item = PLUS } : loop (step 1 end) xs

  d1:xs0 | isDigit d1 -> do
    let (ds,xs) = span isDigit xs0
    let n = read @Int (d1:ds)
    let end = step (length ds) start
    L { start, end = start, item = NUMBER n } : loop (step 1 end) xs

  c:_ -> do
    [ L { start, end = start, item = UNEXPECTED c } ]
  [] ->
    []
