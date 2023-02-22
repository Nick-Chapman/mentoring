module Bytecode (main) where

main :: IO ()
main = do
  let code :: Code = [PUSH 7, DUP, PUSH 1, SWAP, SUB, MUL]
  print (expect 42 (runBC code))

  -- ( 3^2 + (5 - 4) )^2
  let exp :: Exp = Square (Add (Square (Num 3)) (Sub (Num 5) (Num 4)))
  print (expect 100 (eval exp))

  let compiled :: Code = compile exp
  print compiled
  print (expect 100 (runBC compiled))

_compile :: Exp -> Code
_compile = comp
  where
    comp = \case
      Num n -> [PUSH n]
      Add e1 e2 -> compile e2 ++ compile e1 ++ [ADD]
      Sub e1 e2 -> compile e2 ++ compile e1 ++ [SUB]
      Square e -> compile e ++ [DUP,MUL]

compile :: Exp -> Code
compile = comp []
  where
    comp :: [BC] -> Exp -> [BC]
    comp acc = \case
      Num n -> PUSH n : acc
      Add e1 e2 -> comp (comp (ADD:acc) e1) e2
      Sub e1 e2 -> comp (comp (SUB:acc) e1) e2
      Square e -> comp (DUP:MUL:acc) e

data Exp = Num Int | Add Exp Exp | Sub Exp Exp | Square Exp

eval :: Exp -> Int
eval = \case
  Num n -> n
  Add e1 e2 -> eval e1 + eval e2
  Sub e1 e2 -> eval e1 - eval e2
  Square e -> let n = eval e in n*n

type Code = [BC]
data BC = ADD | SUB | MUL | DUP | SWAP | PUSH Int deriving Show

runBC :: [BC] -> Int
runBC code = loop (code,[])
  where
    loop :: ([BC],[Int])-> Int
    loop = \case
      ([], result:_) -> result
      (ADD:code, v1:v2:vs) -> loop (code, (v1+v2) : vs)
      (SUB:code, v1:v2:vs) -> loop (code, (v1-v2) : vs)
      (MUL:code, v1:v2:vs) -> loop (code, (v1*v2) : vs)
      (DUP:code,     v:vs) -> loop (code,  v : v  : vs)
      (SWAP:code,v1:v2:vs) -> loop (code,  v2: v1 : vs)
      (PUSH v:code,    vs) -> loop (code,      v  : vs)
      _ -> error "bad"

expect :: (Eq a, Show a) => a -> a -> a
expect a b = if a == b then a else
  error ("expect failed: " ++ show a ++ " not same as: " ++ show b)
