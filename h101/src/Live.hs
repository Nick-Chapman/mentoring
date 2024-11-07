
module Live where

import Control.Monad (ap,liftM)
import Text.Printf (printf)

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

-- magic so we can use do-syntax & the "return" function
instance Functor M where fmap = liftM
instance Applicative M where pure = Ret; (<*>) = ap
instance Monad M where (>>=) = Bind


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

{-
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
-}


-- sort3 example making use the emitter monad...
compareAndSwap :: (Int,Int) -> Emitter (Int,Int)
compareAndSwap (x,y) = do
  emit (show ("comparing",x,y))
  if x < y
    then return (x,y)
    else do
      emit "Swap"
      Tick
      return (y,x)

sort3 :: (Int,Int,Int) -> Emitter (Int,Int,Int)
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
  let (res,messages_or_n) = run resM
  print (example,"-->",res, messages_or_n)


----------------------------------------------------------------------

-- magic so we can use do-syntax & the "return" function
instance Functor Emitter where fmap = liftM
instance Applicative Emitter where pure = returnE; (<*>) = ap
instance Monad Emitter where (>>=) = bindE


--data Emitter a = Emitter1 a [String]

{-
returnE :: a -> Emitter a
returnE a = Emitter1 a []

emit :: String -> Emitter ()
emit m = Emitter1 () [m]

run :: Emitter a -> (a,[String])
run (Emitter1 a ms) = (a,ms)

bindE :: Emitter a -> (a -> Emitter b) -> Emitter b
bindE (Emitter1 a ms1) f = do
  let (Emitter1 b ms2) = f a
  Emitter1 b (ms1++ms2)
-}



data Emitter a where
  Ret :: a -> Emitter a
  Bind :: Emitter a -> (a -> Emitter b) -> Emitter b
  Tick :: Emitter ()

emit :: String -> Emitter ()
emit _m = pure ()

returnE :: a -> Emitter a
returnE a = Ret a

bindE :: Emitter a -> (a -> Emitter b) -> Emitter b
bindE = Bind

{-
run :: Emitter a -> (a,Int)
run m0 = undefined m0 loop
  where
    loop = \case
      Ret a -> undefined a
      Bind m g -> undefined m g
      Tick -> undefined
-}

run :: Emitter a -> (a,Int)
run m0 = loop m0 0
  where
    loop :: Emitter a -> Int ->(a,Int)
    loop = \case
      Ret a -> \n -> (a,n)
      Tick -> \n -> ((),n+1)
      Bind m g -> \n0 -> do
        let (a,n1) = (loop m) n0
        let (b,n2) = loop (g a) n1
        (b,n2)
