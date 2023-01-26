module Interpreter (main) where

import Prelude hiding (lookup)

import qualified Data.Map as Map (fromList,lookup)
import Data.Map (Map)

main :: IO ()
main = do
  --let sam = Add (Lit 1) (Add (Lit 2) (Lit 3))
  let sam = Add (Lit 1) (Add (Var "x") (Lit 3))
  print sam
  print (eval env0 sam)


-- represent Env using Maps
data Env = Env (Map Identifier Value)

env0 :: Env
env0 = Env (Map.fromList [("x",100)])

lookup :: Env -> Identifier -> Value
lookup (Env m) x =
  case Map.lookup x m of
    Just v -> v
    Nothing -> error "sdfkjhsdkj"


{-
-- represent Env using functions
data Env = EnvCC (Identifier -> Value)

env0 :: Env
env0 = EnvCC (\x -> case x of
                 "x" -> 100
                 "y" -> 200
                 _ -> 42)

lookup :: Env -> Identifier -> Value
lookup (EnvCC f) x = f x
-}


type Value = Int
type Identifier = String

eval :: Env -> Exp -> Value
eval env = \case
  Lit n -> n
  Var x -> lookup env x
  Add l r -> eval env l + eval env r

data Exp
  = Lit Int
  | Var Identifier
  | Add Exp Exp

instance Show Exp where
  show :: Exp -> String
  show = \case
    Lit n -> show n
    Var x -> x
    Add l r -> "(" ++ show l ++ "+" ++ show r ++ ")"
