module Interpreter (main) where

import Data.Map (Map)
import Prelude hiding (lookup)
import Text.Printf (printf)
import qualified Data.Map as Map (fromList,lookup,insert)

main :: IO ()
main = do
  mapM_ run sams
  where
    run :: (Exp,Int) -> IO ()
    run (sam,expected) = do
      putStr (show sam ++ " : ")
      let actual = eval env0 sam
      let ok = (expected==actual)
      let msg = if ok then printf "PASS: %d" actual else printf "FAIL: expected=%d, but got=%d" expected actual
      putStrLn msg

    sams :: [(Exp,Int)]
    sams =
      [ (Add (Lit 1) (Add (Lit 2) (Lit 3))
        , 6
        )
      , (Let "x" (Sub (Lit 10) (Lit 3))
         (Mul (Var "x") (Var "x"))
        , 49)
      , (Let "x" (Lit 5) (Mul (Let "x" (Add (Var "x") (Var "x")) (Sub (Var "x") (Lit 1))) (Var "x"))
        , 45)
      , (Let "x" (Lit 5) (Let "y" (Add (Var "x") (Lit 1)) (Mul (Var "x") (Var "y")))
        , 30)
      ]

data Env = Env (Map Identifier Value)

env0 :: Env
env0 = Env (Map.fromList [])

extend :: Env -> Identifier -> Value -> Env
extend (Env m) x v = Env (Map.insert x v m)

lookup :: Env -> Identifier -> Value
lookup (Env m) x =
  case Map.lookup x m of
    Just v -> v
    Nothing -> error (show ("lookup",x))

type Value = Int
type Identifier = String

eval :: Env -> Exp -> Value
eval env = \case
  Lit n -> n
  Var x -> lookup env x
  Add l r -> eval env l + eval env r
  Sub l r -> eval env l - eval env r
  Mul l r -> eval env l * eval env r
  Let x rhs body -> do
    let v = eval env rhs
    let env' = extend env x v
    eval env' body

data Exp
  = Lit Int
  | Var Identifier
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Let Identifier Exp Exp

instance Show Exp where
  show :: Exp -> String
  show = \case
    Lit n -> show n
    Var x -> x
    Add l r -> bin "+" l r
    Sub l r -> bin "-" l r
    Mul l r -> bin "*" l r
    Let x rhs body -> "(let " ++ x ++ " = " ++ show rhs ++ " in " ++ show body ++ ")"

    where bin op l r = "(" ++ show l ++ op ++ show r ++ ")"
