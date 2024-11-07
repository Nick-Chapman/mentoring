
module Master2 where

import Text.Printf (printf)
import Control.Monad (ap,liftM)

-- sort3 example making use the emitter monad...
compareAndSwap :: (Int,Int) -> Emitter (Int,Int)
compareAndSwap (x,y) = do
  --n <- HowMany
  emit (show ("comparing",x,y))
  if x < y
    then return (x,y)
    else do
      emit "Swap"
      --Tick
      return (y,x)

sort3 :: (Int,Int,Int) -> Emitter (Int,Int,Int)
sort3 (a,b,c) = do
  (a,b) <- compareAndSwap (a,b)
  (b,c) <- compareAndSwap (b,c)
  (a,b) <- compareAndSwap (a,b)
  return (a,b,c)

instance Functor Emitter where fmap = liftM
instance Applicative Emitter where pure = returnE; (<*>) = ap
instance Monad Emitter where (>>=) = bindE

-- running the example
main :: IO ()
main = do
  printf "hey!\n"
  let example = (8,9,7)
  let resM = sort3 example
  let (res,messages) = run resM
  print (example,"-->",res, messages)

data Emitter a where
  Ret :: a -> Emitter a
  Bind :: Emitter a -> (a -> Emitter b) -> Emitter b
  Emit :: String -> Emitter ()
  Tick :: Emitter ()
  HowMany :: Emitter Int

emit :: String -> Emitter ()
emit m = Emit m

returnE :: a -> Emitter a
returnE a = Ret a

bindE :: Emitter a -> (a -> Emitter b) -> Emitter b
bindE = Bind

run :: Emitter a -> (a,[String])
run m0 = loop m0
  where
    loop :: Emitter a -> (a,[String])
    loop m0 =
      case m0 of
      Ret a -> (a,[])
      Emit s -> ((), [s])
      Bind m g -> do
        let (a,xs1) = loop m
        let mb = g a
        let (b,xs2) = loop mb
        (b , xs1 ++ xs2)
      Tick -> undefined
      HowMany -> undefined

{-
data Emitter a = Emitter1 a [String]

returnE :: a -> Emitter a
returnE a = Emitter1 a []

emit :: String -> Emitter ()
emit s = Emitter1 () [s]

run :: Emitter a -> (a,[String])
run (Emitter1 a ss) = (a,ss)

bindE :: Emitter a -> (a -> Emitter b) -> Emitter b
bindE (Emitter1 a ss1) f = do
  let Emitter1 b ss2 = f a
  Emitter1 b (ss1++ss2)
-}


data Ticker a = Ticker (Int -> (a,Int))
