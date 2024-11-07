module Master3 where

import Control.Monad (ap,liftM)


instance Functor SM where fmap = liftM
instance Applicative SM where pure = returnSM; (<*>) = ap
instance Monad SM where (>>=) = bindSM

main :: IO ()
main = do
  print tree1
  let t2 = runSM (label tree1)
  print t2

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show = \case
    Leaf a -> show a
    Node t1 t2 -> "(" ++ show t1 ++ "," ++ show t2 ++ ")"

tree1 :: Tree String
tree1 = Node (Node (Leaf "a") (Leaf "b")) (Node (Leaf "c") (Leaf "d"))


label :: Tree a -> SM (Tree (a,Int))
label = \case
  Leaf a -> do
    n <- tick
    pure (Leaf (a,n))

  Node t1 t2 -> do
    lt1 <- label t1
    lt2 <- label t2
    pure (Node lt1 lt2)

tick :: SM Int
tick = do
  n <- getSM
  setSM (n+1)
  pure n

{-
getSM :: SM Int
getSM = Get

setSM :: Int -> SM ()
setSM = Set

returnSM :: a -> SM a
returnSM a = Ret a

bindSM :: SM a -> (a -> SM b) -> SM b
bindSM = Bind

data SM a where
  Ret :: a -> SM a
  Bind :: SM a -> (a -> SM b) -> SM b
  Get :: SM Int
  Set :: Int -> SM ()

runSM :: SM a -> a
runSM m = let (a,_) = run m 1 in a
  where
    run :: SM a -> Int -> (a,Int)
    run = \case
      Ret a -> \n -> (a,n)
      Get -> \n -> (n,n)
      Set n -> \_ -> ((),n)
      Bind m g -> \n0 -> do
        let (a,n1) = run m n0
        let (b,n2) = run (g a) n1
        (b,n2)
-}



data SM a = SM (Int -> (a,Int))

getSM :: SM Int
getSM = SM (\n -> (n,n))

setSM :: Int -> SM ()
setSM n = SM (\_ -> ((),n))

returnSM :: a -> SM a
returnSM a = SM (\n -> (a,n))

bindSM :: SM a -> (a -> SM b) -> SM b
bindSM (SM f1) g =
  SM (\n -> do
        let (a,n1) = f1 n
        let SM f2 = g a
        f2 n1)

runSM :: SM a -> a
runSM (SM f) = let (a,_) = f 1 in a


{-
data SM a

getSM :: SM Int
setSM :: Int -> SM ()
returnSM :: a -> SM a
bindSM :: SM a -> (a -> SM b) -> SM b
runSM :: SM a -> a


getSM = undefined
setSM = undefined
returnSM = undefined
bindSM = undefined
runSM = undefined
-}

{-data SM a

getSM :: SM Int
getSM = undefined

setSM :: Int -> SM ()
setSM = undefined

returnSM :: a -> SM a
returnSM = undefined

runSM :: SM a -> a
runSM = undefined

bindSM :: SM a -> (a -> SM b) -> SM b
bindSM = undefined
-}
