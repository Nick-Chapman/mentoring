module Ast (Program(..),Exp(..)) where

data Program = Program { main :: Exp } deriving Show

data Exp = Lit Int deriving Show
