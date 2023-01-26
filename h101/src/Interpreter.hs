module Interpreter (main) where

import Prelude hiding (lookup)
import Text.Printf

import qualified Data.Map as Map (fromList,lookup)
import Data.Map (Map)

main :: IO ()
main = do
  mapM_ run sams
  where
    run :: (Exp,Int) -> IO ()
    run (sam,expected) = do
      putStr (show sam ++ " : ")
      let actual = eval env0 sam
      let ok = (expected==actual)
      let msg = if ok then "PASS" else printf "FAIL: expected=%d, but got=%d" expected actual
      putStrLn msg

    sams :: [(Exp,Int)]
    sams =
      [ (Add (Lit 1) (Add (Lit 2) (Lit 3))
        , 6
        )
      , (Add (Lit 1) (Add (Var "x") (Lit 3))
        , 104
        )
      ]

data Env = Env (Map Identifier Value)

env0 :: Env
env0 = Env (Map.fromList [("x",100)])

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
