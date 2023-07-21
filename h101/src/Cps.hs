
module Cps(main) where

main :: IO ()
main = do
  print "cps"
  let _ = print (fact 6)
  print (fib 10)

{-fact :: Int -> Int
fact n =
  if n == 0
  then 1
  else n * fact (n-1)-}

{-fact :: Int -> Int
fact n = factA (n,1)

factA :: (Int,Int) -> Int
factA (n,acc) =
  if n == 0
  then acc
  else factA (n-1, n*acc)-}

fact :: Int -> Int
fact n = do
  factC (n, \res -> res)

factC :: (Int,Int->Int) -> Int
factC (n,k) =
  if n == 0
  then k 1
  else do
    let k2 res = k (n * res)
    factC (n-1, k2)

{-fib :: Int -> Int
fib n =
  if n < 2
  then n
  else fib (n-1) + fib (n-2)
-}

{-fib :: Int -> Int
fib n = fibC (n, \res -> res)

fibC :: (Int,Int->Int) -> Int
fibC (n,k) =
  if n < 2
  then k n
  else do
    let
      k1 res1 = do
        let k2 res2 = k (res1 + res2)
        fibC (n-2, k2)
    fibC (n-1, k1)-}


fib :: Int -> Int
fib n = fibD (n,KDone)

fibD :: (Int,FK) -> Int
fibD (n,k) =
  if n < 2
  then apply k n
  else fibD (n-1, KOne n k)

apply :: FK -> Int -> Int
apply = \case
  KDone -> \res -> res
  KOne n k -> \res1 -> fibD (n-2, KTwo k res1)
  KTwo k res1 -> \res2 -> apply k (res1 + res2)

data FK
  = KDone
  | KOne Int FK
  | KTwo FK Int
