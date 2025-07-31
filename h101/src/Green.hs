
module Green (main) where

import Control.Monad (ap,liftM)

{-
main :: IO ()
main = do
  print (subsqr 3 4)

subsqr :: Int -> Int -> Int
subsqr x y = subsqr' x y (\ans -> ans)


subsqr' :: Int -> Int -> (Int -> r) -> r
subsqr' x y k = do
{-  let k1 xx = do
        let k2 yy = k (xx+yy)
        sqr' y k2
  sqr' x k1-}

  sqr' x (\xx -> sqr' y (\yy -> k (xx+yy)))
  --sqr' x $ \xx -> sqr' y $ \yy -> k (xx+yy)

sqr' :: Int -> (Int -> r) -> r
sqr' n k = k (n*n)
-}
----------------------------------------------------------------------

main :: IO ()
main = runM $ do
  xs <- thingA
  Print (show ("done",xs))

thingA :: M [Int]
thingA = do
  a <- thingAsub 'a'
  b <- thingAsub 'b'
  c <- thingAsub 'c'
  pure [a,b,c]

thingAsub :: Char -> M Int
thingAsub c = do
  r <- Fresh
  Print (show ("thingA",c,r))
  pure r

----------------------------------------------------------------------

instance Monad M where (>>=) = Bind
instance Applicative M where pure = Pure; (<*>) = ap
instance Functor M where fmap = liftM

data M a where -- GADT
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  Print :: String -> M ()
  Fresh :: M Int

data State = State { u :: Int }

runM :: M () -> IO ()
runM m = do
  (State{u},()) <- loop m state0
  putStrLn (show ("final",u))
  pure ()

  where
    state0 :: State = State { u = 0 }

    loop :: M a -> State -> IO (State,a)
    loop m s = case m of
      Pure a -> do
        pure (s,a)

      Bind m f -> do
        (s',a) <- loop m s
        loop (f a) s'

      Print str -> do
        putStrLn ("PRINT:" ++ str)
        pure (s,())

      Fresh -> do
        let State{u} = s
        let s' = State { u = u+1 }
        pure (s',u)


_runM :: M () -> IO () -- TODO tomorrow
_runM m = undefined m state0 loop

  where
    state0 :: State = State { u = 0 }

    loop :: M a -> State -> (State -> a -> IO ()) -> IO ()
    loop m _s _k = case m of
      Pure a -> do
        undefined a

      Bind m f -> do
        undefined m f

      Print str -> do
        undefined str

      Fresh -> do
        undefined
