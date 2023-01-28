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
    f = Var"f"

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

      , (Let "sq" (Lam "x" (Mul x x))
         (App (Var "sq") (App (Var "sq") (Add (Lit 1) (Lit 2))))
        , VI 81)

      , (Let "sub" (Lam "x" (Lam "y" (Sub x y)))
         (App (App (Var "sub") (Lit 10)) (Lit 3)),
         VI 7)

      , (Let "thrice" (Lam "f" (Lam "x" (App f (App f (App f x)))))
         (Let "dec" (Lam "x" (Sub x (Lit 1)))
          (App (App (App (Var "thrice") (Var "thrice")) (Var "dec")) (Lit 0))) -- -27
        , VI (-27))

      , (Let "x" (Lit (-5)) (If (Less x (Lit 0)) (Sub (Lit 0) x) x),
         VI 5)
      , (Let "x" (Lit (5)) (If (Less x (Lit 0)) (Sub (Lit 0) x) x),
         VI 5)

      , (Let "fact"
          (App (Var "fix")
            (Lam "fact"
              (Lam "x"
                (If (Less x (Lit 1)) (Lit 1)
                  (Mul x (App (Var "fact") (Sub x (Lit 1))))
                ))))
          (App (Var "fact") (Lit 5))
        , VI 120)

      ]

data Value
  = VI Int
  | VS String
  | VB Bool
  | VF Env Identifier Exp
  deriving (Eq,Show)

type Identifier = String
data Exp
  = Lit Int
  | LitS String
  | Var Identifier
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Less Exp Exp
  | Concat Exp Exp
  | Let Identifier Exp Exp
  | Lam Identifier Exp
  | App Exp Exp
  | If Exp Exp Exp
  deriving (Eq)


eval :: Env -> Exp -> Value
eval env = \case
  Lit n -> VI n
  LitS s -> VS s
  Var x -> lookup env x
  Add l r -> addV (eval env l) (eval env r)
  Sub l r -> subV (eval env l) (eval env r)
  Mul l r -> mulV (eval env l) (eval env r)
  Less l r -> lessV (eval env l) (eval env r)
  Concat l r -> concatV (eval env l) (eval env r)
  Let x rhs body -> do
    let v = eval env rhs
    let env' = extend env x v
    eval env' body
  App l r -> apply (eval env l) (eval env r)
  Lam x e -> VF env x e
  If i t e -> if getB (eval env i) then eval env t else eval env e

apply :: Value -> Value -> Value
apply f v = case f of
  VF env x body -> do
    let env' = extend env x v
    let res = eval env' body
    res
  _ ->
    error (printf "apply: value in function position not a function: %s " (show f))

addV,subV,mulV,concatV,lessV :: Value -> Value -> Value
addV v1 v2 = VI (getI v1 + getI v2)
subV v1 v2 = VI (getI v1 - getI v2)
mulV v1 v2 = VI (getI v1 * getI v2)
concatV v1 v2 = VS (getS v1 ++ getS v2)
lessV v1 v2 = VB (getI v1 < getI v2)

getS :: Value -> String
getS = \case
  VS s -> s
  v -> error (printf "getS: value not a string: %s " (show v))

getI :: Value -> Int
getI = \case
  VI i -> i
  v -> error (printf "getI: value not an int: %s " (show v))

getB :: Value -> Bool
getB = \case
  VB b -> b
  v -> error (printf "getB: value not a bool: %s " (show v))

data Env = Env (Map Identifier Value) deriving (Eq,Show)

env0 :: Env
env0 = Env (Map.fromList [("fix",fixValue)])

fixValue :: Value
fixValue = VF env0 "uf" body
  where
    -- fix uf ---> uf (fix uf)
    body :: Exp
    body = App (Var "uf") (App (Var "fix") (Var "uf"))

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
    Less l r -> bin " < " l r
    Concat l r -> bin " ++ " l r
    Let x rhs body -> "(let " ++ x ++ " = " ++ show rhs ++ " in " ++ show body ++ ")"
    App l r -> "(" ++ show l ++ " " ++ show r ++ ")"
    Lam x e -> "(\\" ++ x ++ " -> " ++ show e ++ ")"
    If i t e -> "(if " ++ show i ++ " then " ++ show t ++ " else " ++ show e ++ ")"
    where
      bin op l r = "(" ++ show l ++ op ++ show r ++ ")"

