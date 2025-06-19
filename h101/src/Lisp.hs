module Lisp (main) where

import Control.Monad (ap,liftM)
import Data.IORef (IORef,newIORef,readIORef,writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  v <- runM (eval [] example)
  print v

-- no parser: construct the AST for our fav example by hand
example :: Exp
example = App [App [App[thrice,thrice],inc],Num 100]
  where
  thrice = Lam [f] (Lam [x] (App [Var f,App [Var f,App [Var f, Var x]]]))
  inc = Lam [y] (App [add,Var y,Num 1])
  add = Lam [x,y] (Add [Var x, Var y])
  x = "x"
  y = "y"
  f = "f"

-- values
data Value = VNil | VNum Int | VClo Closure deriving Show
data Closure = Closure Env [Id] Exp
instance Show Closure where show Closure{} = "[closure]"

-- Expression AST
type Id = String
data Exp
  = Num Int
  | Var Id
  | Add [Exp]
  | App [Exp]
  | Lam [Id] Exp
  | Set Id Exp

-- eval/apply evaluation
-- delegating some semantics to "M" -- MLook/MSet/MExtend

eval :: Env -> Exp -> M Value
eval env = \case
  Num n -> pure (VNum n)
  Var x -> MLook env x
  Add exps -> do
    vs <- mapM (eval env) exps
    pure (VNum (sum [ n | VNum n <- vs ]))
  App [] -> error "App[]"
  App (f:args) -> do
    f <- eval env f
    args <- mapM (eval env) args
    apply f args
  Lam xs body -> do
    pure (VClo (Closure env xs body))
  Set name exp -> do
    value <- eval env exp
    MSet env name value
    pure VNil

apply :: Value -> [Value] -> M Value
apply = \case
  VNil{} -> error "apply/Nil"
  VNum{} -> error "apply/Num"
  VClo (Closure env xs body) -> \vs -> do
    env' <- MExtend env xs
    sequence_ [ MSet env' x v | (x,v) <- zip xs vs ]
    eval env' body

-- M captures the interface to our tricky mutable environment semantics.
data M a where
  MRet :: a -> M a
  MBind :: M a -> (a -> M b) -> M b
  MLook :: Env -> Id -> M Value
  MSet :: Env -> Id -> Value -> M ()
  MExtend :: Env -> [Id] -> M Env

-- guess what, M's a monad.
instance Functor M where fmap = liftM
instance Applicative M where pure = MRet; (<*>) = ap
instance Monad M where (>>=) = MBind

-- The representation for mutable environments.
type Env = [Frame]
type Frame = Map Id Slot
type Slot = IORef Value

-- "runM" captures the semantics (in terms of the semantcs of IO)
runM :: M a -> IO a
runM = loop
  where
    loop :: M a -> IO a
    loop = \case
      MRet x -> pure x
      MBind m f -> do b <- loop m; loop (f b)
      MLook env x -> do -- find the slot and read it
        let slot = lookupSlot x env
        readIORef slot
      MSet env x v -> do -- find the slot and write to it
        let slot = lookupSlot x env
        writeIORef slot v
      MExtend env xs -> do
        slots <- sequence [ newIORef undefined | _ <- xs ]
        let newFrame = Map.fromList (zip xs slots)
        pure (newFrame:env)

-- The core of the semantics is the lookup to find a slot
lookupSlot :: Id -> Env -> Slot
lookupSlot x = \case
  [] -> error (show ("lookupSlot",x))
  frame:parentFrames ->
    case Map.lookup x frame of
      Just v -> v
      Nothing -> lookupSlot x parentFrames
