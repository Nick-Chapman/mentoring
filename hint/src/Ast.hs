module Ast (Program(..),Exp(..)) where

data Program = Program { main :: Exp } deriving Show

data Exp
  = Lit Int
  | Add Exp Exp
  deriving Show
