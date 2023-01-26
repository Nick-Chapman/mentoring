module Interpreter (main) where

import Data.Map (Map)
import Prelude hiding (lookup)
import Text.Printf (printf)
import qualified Data.Map as Map (fromList,lookup,insert)

main :: IO ()
main = do
  mapM_ run sams
  where
    run :: (Exp,Value) -> IO ()
    run (sam,expected) = do
      putStr (show sam ++ " : ")
      let actual = eval env0 sam
      let ok = (expected==actual)
      let msg =
            if ok then printf "PASS: %s" (show actual)
            else printf "FAIL: expected=%s, but got=%s" (show expected) (show actual)
      putStrLn msg

    x = Var"x"
    y = Var"y"

    sams :: [(Exp,Value)]
    sams =
      [ (Add (Lit 1) (Add (Lit 2) (Lit 3))
        , VI 6
        )
      , (Let "x" (Sub (Lit 10) (Lit 3))
         (Mul x x)
        , VI 49)
      , (Let "x" (Lit 5) (Mul (Let "x" (Add x x) (Sub x (Lit 1))) x)
        , VI 45)
      , (Let "x" (Lit 5) (Let "y" (Add x (Lit 1)) (Mul x y))
        , VI 30)
      , (Let "hw" (Concat (LitS "hello, ") (LitS "world")) (Concat (Var "hw") (LitS "!!"))
        , VS "hello, world!!")

      , (Lam "x" (Mul x x)
        , VI 0)

      , (Let "sq" (Lam "x" (Mul x x)) (App (Var "sq") (Lit 3))
        , VI 0)
      ]

data Value = VI Int | VS String | VF deriving (Eq,Show)

type Identifier = String
data Exp
  = Lit Int
  | LitS String
  | Var Identifier
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Concat Exp Exp
  | Let Identifier Exp Exp
  | Lam Identifier Exp
  | App Exp Exp


eval :: Env -> Exp -> Value
eval env = \case
  Lit n -> VI n
  LitS s -> VS s
  Var x -> lookup env x
  Add l r -> addV (eval env l) (eval env r)
  Sub l r -> subV (eval env l) (eval env r)
  Mul l r -> mulV (eval env l) (eval env r)
  Concat l r -> concatV (eval env l) (eval env r)
  Let x rhs body -> do
    let v = eval env rhs
    let env' = extend env x v
    eval env' body
  App{} -> undefined
  Lam{} -> undefined

addV,subV,mulV,concatV :: Value -> Value -> Value
addV v1 v2 = VI (getI v1 + getI v2)
subV v1 v2 = VI (getI v1 - getI v2)
mulV v1 v2 = VI (getI v1 * getI v2)
concatV v1 v2 = VS (getS v1 ++ getS v2)

getS :: Value -> String
getS = \case
  VS s -> s
  v -> error (printf "getS: value not a string: %s " (show v))

getI :: Value -> Int
getI = \case
  VI i -> i
  v -> error (printf "getI: value not an int: %s " (show v))


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


instance Show Exp where
  show :: Exp -> String
  show = \case
    Lit n -> show n
    LitS s -> show s
    Var x -> x
    Add l r -> bin "+" l r
    Sub l r -> bin "-" l r
    Mul l r -> bin "*" l r
    Concat l r -> bin " ++ " l r
    Let x rhs body -> "(let " ++ x ++ " = " ++ show rhs ++ " in " ++ show body ++ ")"
    App l r -> "(" ++ show l ++ " " ++ show r ++ ")"
    Lam x e -> "(\\" ++ x ++ " -> " ++ show e ++ ")"
    where
      bin op l r = "(" ++ show l ++ op ++ show r ++ ")"

