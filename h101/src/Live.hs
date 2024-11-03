
module Live where

import Text.Printf (printf)

-- magic so we can use do-syntax & the "return" function
import Control.Monad (ap,liftM)
instance Functor M where fmap = liftM
instance Applicative M where pure = Ret; (<*>) = ap
instance Monad M where (>>=) = Bind


-- Two syntaxes for defining a data type...
{-
date Tree a = Leaf a | Node (Tree a) (Tree a)

date Tree _
  Leaf :: a -> Tree a
  Node :: Tree a -> Tree a -> Tree a

myTree = Node (Node (Leaf 12) (Leaf 34)) (Leaf 56)

myFunc t =
  case t of
    Node l r -> ...
    Leaf x -> ...
-}


-- before there were monads...
{-
sort3basic :: (Int,Int,Int) -> (Int,Int,Int)
sort3basic (a,b,c) = do
  let (a1,b1) = compareAndSwap (a,b)
  let (b2,c1) = compareAndSwap (b1,c)
  let (a2,b3) = compareAndSwap (a1,b2)
  (a2,b3,c1)

compareAndSwap :: (Int,Int) -> (Int,Int)
compareAndSwap (x,y) =
  if x < y
  then (x,y) -- already in order
  else (y,x) -- swap
-}


{-
-- the original plain monad
data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b

-- running the plain monad
run :: M x -> x
run m0 = case m0 of
  Ret a -> a
  Bind m g -> do
    let a = run m
    let mb = g a
    let b = run mb
    b
  Emit _s -> ()
-}


-- The emitter monad...
data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Emit :: String -> M ()

-- running the emitter monad
run :: M x -> (x,[String])
run m0 = case m0 of
  Ret a -> (a,[])
  Emit s -> ((), [s])
  Bind m g -> do
    let (a,xs1) = run m
    let mb = g a
    let (b,xs2) = run mb
    (b , xs1 ++ xs2)

-- sort3 example making use the emitter monad...
compareAndSwap :: (Int,Int) -> M (Int,Int)
compareAndSwap (x,y) = do
  Emit (show ("comparing",x,y))
  if x < y
    then Ret (x,y)
    else do
      Emit "Swap"
      Ret (y,x)

sort3 :: (Int,Int,Int) -> M (Int,Int,Int)
sort3 (a,b,c) = do
  (a,b) <- compareAndSwap (a,b)
  (b,c) <- compareAndSwap (b,c)
  (a,b) <- compareAndSwap (a,b)
  return (a,b,c)

-- running the example
main :: IO ()
main = do
  printf "hey!\n"
  let example = (8,9,7)
  let resM = sort3 example
  let (res,messages) = run resM
  print (example,"-->",res, messages)
