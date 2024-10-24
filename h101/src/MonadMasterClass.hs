module MonadMasterClass where

import Text.Printf (printf)
import Control.Monad (ap,liftM)


-- monads: bind/return ("pure" alternative spelling of "return"

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
data Tree1 a
  = Leaf1 a
  | Node1 (Tree1 a) a (Tree1 a)

data Tree a where
  Leaf :: a -> Tree a
  Node :: Tree a -> a -> Tree a -> Tree a
-}

{-
sort3basicM :: (Int,Int,Int) -> M (Int,Int,Int)
sort3basicM (a,b,c) = do
  compareAndSwapM (a,b) `BindM` \(a,b) ->
    compareAndSwapM (b,c) `BindM` \(b,c) ->
      compareAndSwapM (a,b) `BindM` \(a,b) -> RetM (a,b,c)
-}
{-
runM :: M a -> a
runM = \case
  RetM a -> a
  BindM ma f -> do
    let a = runM ma
    let mb = f a
    let b = runM mb
    b
-}

{-
data M a where
  RetM :: a -> M a
  BindM :: M a -> (a -> M b) -> M b

runM :: M a -> a
runM = \case
  RetM a -> a
  BindM ma f -> do
    let a = runM ma
    let mb = f a
    runM mb
-}

main :: IO ()
main = do
  printf "*monad master class*\n"
  sequence_
    [ print (example,"-->",res, ":", actions)
    | example <- [ (1,2,3), (8,9,7), (7,5,3) ]
    , let (res,actions) = sort3 example
    ]
  where
    sort3 x = runM (sort3basic x)

sort3basic :: (Int,Int,Int) -> M (Int,Int,Int)
sort3basic (a,b,c) = do
  (a,b) <- compareAndSwap (a,b)
  (b,c) <- compareAndSwap (b,c)
  (a,b) <- compareAndSwap (a,b)
  pure (a,b,c)

compareAndSwap :: (Int,Int) -> M (Int,Int)
compareAndSwap (x,y) = do
  Emit (Compare x y)
  if x < y then pure (x,y) else do
    Emit Swap
    pure (y,x)

data Action = Compare Int Int | Swap deriving Show

instance Functor M where fmap = liftM
instance Applicative M where pure = Ret; (<*>) = ap
instance Monad M where (>>=) = Bind

data M a where
  Ret :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Emit :: Action -> M ()

runM :: M x -> (x, [Action])
runM = \case
  Ret a -> (a, [])
  Emit action -> ((), [action])
  Bind ma f -> do
    let (a,xs1) = runM ma
    let mb = f a
    let (b,xs2) = runM mb
    (b, xs1 ++ xs2)
