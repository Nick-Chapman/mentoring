
module PrepGreen (main) where

import Control.Monad (ap,liftM)

import Data.IORef (IORef,newIORef,readIORef,writeIORef)

main :: IO ()
main = do
  runM $ do
    (xs,ys) <- Par (rep 5 thingA) (rep 7 thingB)
    Print ("done:" ++ show (xs,ys))
  putStrLn "main complete"

rep :: Int -> M a -> M [a]
rep n = sequence . replicate n

thingA :: M Int
thingA = do
  n <- GetCounter
  Print ("thingA: " ++ show n)
  Yield
  pure n

thingB :: M Int
thingB = do
  (n,m) <- getPairCounters
  Print ("thingB: " ++ show (n,m))
  Yield
  pure n

getPairCounters :: M (Int,Int)
getPairCounters = do
  i <- GetCounter
  j <- GetCounter
  pure (i,j)

instance Monad M where (>>=) = Bind
instance Functor M where fmap = liftM
instance Applicative M where pure = Pure; (<*>) = ap

data M a where
  Pure :: a -> M a
  Bind :: M a -> (a -> M b) -> M b
  GetCounter :: M Int
  Print :: String -> M ()
  Yield :: M ()
  Par :: M a -> M b -> M (a,b)

data State = State { u :: Int, pend :: [Job] }

data Job where
  FromMK :: M a -> (State -> a -> IO ()) -> Job

runM :: M () -> IO ()
runM m = loop m State { u = 0, pend = [] } kFinal

  where
    kFinal :: (State -> () -> IO ())
    kFinal State{pend} () = do
      case pend of
        [] -> pure ()
        _:_ -> error "unexpected jobs left over"

    continue :: State -> IO ()
    continue s@State{pend} = do
      case pend of
        [] -> error "no jobs to continue with"
        j1:js -> do
          let s' = s { pend = js }
          case j1 of
            FromMK m1 k1 -> do
              loop m1 s' k1

    suspend :: M a -> (State -> a -> IO ()) -> State -> State
    suspend m k s@State{pend} = do
      let me = FromMK m k
      s { pend = pend ++ [me] }

    loop :: M a -> State -> (State -> a -> IO ()) -> IO ()
    loop m s = case m of
      Pure x -> \k -> do
        k s x
      Bind m f -> \k -> do
        loop m s $ \s a -> loop (f a) s k
      GetCounter -> \k -> do
        let State{u} = s
        k s { u = u+1 } u
      Print mes -> \k -> do
        putStrLn mes;
        k s ()
      Yield -> \k -> do
        let s' = suspend (pure ()) k s
        continue s'
      Par mA mB -> \k -> do
        vA :: IORef (Maybe a) <- newIORef Nothing
        vB :: IORef (Maybe b) <- newIORef Nothing
        let
          kA :: State -> a -> IO ()
          kA s a = do
            readIORef vB >>= \case
              Just b -> k s (a,b)
              Nothing -> do
                writeIORef vA (Just a)
                continue s
        let
          kB :: State -> b -> IO ()
          kB s b = do
            readIORef vA >>= \case
              Just a -> k s (a,b)
              Nothing -> do
                writeIORef vB (Just b)
                continue s

        loop mA (suspend mB kB s) kA
