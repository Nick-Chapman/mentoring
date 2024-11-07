
module New where

import Text.Printf (printf)
import Control.Monad (ap,liftM)

-- sort3 example making use the emitter monad...
compareAndSwap :: (Int,Int) -> Emitter (Int,Int)
compareAndSwap (x,y) = do
  emit (show ("comparing",x,y))
  if x < y
    then return (x,y)
    else do
      emit "Swap"
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
  let (res,messages) = run resM
  print (example,"-->",res, messages)


{-
data Emitter a where
  Ret :: a -> Emitter a
  Bind :: Emitter a -> (a -> Emitter b) -> Emitter b
  Emit :: String -> Emitter ()


emit :: String -> Emitter ()
emit m = Emit m

run :: Emitter a -> (a,[String])
run = undefined

instance Functor Emitter where fmap = liftM
instance Applicative Emitter where pure = returnE; (<*>) = ap
instance Monad Emitter where (>>=) = bindE

returnE :: a -> Emitter a
returnE a = Ret a

bindE :: Emitter a -> (a -> Emitter b) -> Emitter b
bindE = Bind
-}


instance Functor Emitter where fmap = liftM
instance Applicative Emitter where pure = returnE; (<*>) = ap
instance Monad Emitter where (>>=) = bindE

data Emitter a = Emitter1 a [String]

returnE :: a -> Emitter a
returnE = undefined

emit :: String -> Emitter ()
emit = undefined

run :: Emitter a -> (a,[String])
run = undefined

bindE :: Emitter a -> (a -> Emitter b) -> Emitter b
bindE = undefined


