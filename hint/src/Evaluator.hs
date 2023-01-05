module Evaluator (execute) where

import Ast (Program(..),Exp(..))
import Value (Value)

execute :: Program -> Value
execute Program{main} =
  eval main

eval :: Exp -> Value
eval = \case
  Lit n -> n
  Add e1 e2 -> eval e1 + eval e2
  Sub e1 e2 -> eval e1 - eval e2
