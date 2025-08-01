
module Green (main) where

import Control.Monad (ap,liftM)

import Data.IORef (IORef,newIORef,readIORef,writeIORef)

{-
_main :: IO ()
_main = do
  print (subsqr 3 4)

subsqr :: Int -> Int -> Int
subsqr x y = subsqr' x y (\ans -> ans)


subsqr' :: Int -> Int -> (Int -> r) -> r
subsqr' x y k = do

  sqr' x (\xx -> sqr' y (\yy -> k (xx+yy)))

sqr' :: Int -> (Int -> r) -> r
sqr' n k = k (n*n)
-}
----------------------------------------------------------------------

main :: IO ()
main = runM $ do
  (xs,ys) <- Par thingA thingB
  Print (show ("done",xs,ys))

{-par :: M a -> M b -> M (a,b)
par ma mb = do
  a <- ma
  b <- mb
  pure (a,b)-}

thingA :: M [Int]
thingA = do
  sequence [ thingAsub c | c <- "abcde" ]

{-parallel :: [M a] -> M [a]
parallel = \case
  [] -> pure []
--  [m] -> m
  m:ms -> do
    (a,as) <- Par m (parallel ms)
    pure (a:as)-}

thingAsub :: Char -> M Int
thingAsub c = do
  r <- Fresh
  Print (show ("thingA",c,r))
  --Print (show ("thingAA",c,r))
  --Print (show ("thingAAA",c,r))
  Yield
  pure r


thingB :: M [Int]
thingB = do
  x <- thingBsub 'x'
  y <- thingBsub 'y'
  z <- thingBsub 'z'
  pure [x,y,z]

thingBsub :: Char -> M Int
thingBsub c = do
  r <- Fresh
  r2 <- Fresh
  Print (show ("thingB",c,r,r2))
  Yield
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
  Par :: M a -> M b -> M (a,b)
  Yield :: M ()

data State = State { u :: Int, jobs :: [Job] }

runM :: M () -> IO ()
runM m0 = do

  loop m0 state0 kFinal

  where
    kFinal :: State -> () -> IO ()
    kFinal State{u=_} () = do
      --putStrLn (show ("final",u))
      pure ()

    state0 :: State = State { u = 0, jobs = [] }

    loop :: M a -> State -> (State -> a -> IO ()) -> IO ()
    loop m s k = case m of
      Pure a -> do
        k s a

      Bind m f -> do
        loop m s (\s a -> loop (f a) s k)

      Print str -> do
        putStrLn ("PRINT:" ++ str)
        k s ()

      Fresh -> do
        let State{u} = s
        let s' = s { u = u+1 }
        k s' u

      Par ma mb -> do

        vA :: IORef (Maybe a) <- newIORef Nothing
        vB :: IORef (Maybe b) <- newIORef Nothing

        let
          ka :: State -> a -> IO ()
          ka s a = do
            readIORef vB >>= \case
              Just b -> k s (a,b)
              Nothing -> do
                writeIORef vA (Just a)
                restore s

          kb :: State -> b -> IO ()
          kb s b = do
            readIORef vA >>= \case
              Just a -> k s (a,b)
              Nothing -> do
                writeIORef vB (Just b)
                restore s

        let jb = Job mb kb
        let State{jobs} = s
        let s' = s { jobs = jb : jobs }
        loop ma s' ka

      Yield -> do
        let State{jobs} = s
        let me = Job (pure ()) k
        let s' = s { jobs = jobs ++ [me] }
        restore s'

    restore :: State -> IO ()
    restore s = do
        let State{jobs} = s
        case jobs of
          [] -> error "restore[]"
          j1:jobs -> do
            let s' = s { jobs }
            case j1 of
              Job m k ->
                loop m s' k

data Job where
  Job :: M a -> (State -> a -> IO ()) -> Job
