
module PlayMemo (main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State,evalState,get,put,modify)
import System.Clock (TimeSpec(..),getTime,Clock(Monotonic))
import GHC.Int (Int64)
import Text.Printf (printf)
import Data.Array (Ix,array,range,(!))

--import Data.MemoTrie

main :: IO ()
main = do
  print "*play-memo*"
  test "Vanilla" fibV
  -- Memization via Map passed in State monad
  test "Samir(orig)" fibM1
  test "Samir(swapped)" fibM2
  test "Samir(fixed)" fibM3
  test "One mutual def" fibM4
  -- Array based memoization...
  test "A1" fibA1
  pure ()


fibA1 :: Integer -> Integer
fibA1 = fib
  where
    fib :: Integer -> Integer
    fib = memoA (0,100000) $ \case
      0 -> 0
      1 -> 1
      n -> fib (n - 2) + fib (n - 1)


-- array based memoization
memoA :: Ix a => (a,a) -> (a -> b) -> (a -> b)
memoA r = (!) . \f -> array r [ (x,f x) | x <- range r ]


-- one def. bit slower (because we start with memoized?)
fibM4 :: Integer -> Integer
fibM4 = runS . fib
  where
    fib :: Integer -> S Integer
    fib = memoize $ \case
      0 -> return 0
      1 -> return 1
      n -> do
        a <- fib (n - 2)
        b <- fib (n - 1)
        return $ a + b


-- fix broken memoization...
fibM3 :: Integer -> Integer
fibM3 = runS . fib
  where
    fib :: Integer -> S Integer
    fib 0 = return 0
    fib 1 = return 1
    fib n = do
      a <- memoizedFib (n - 2)
      b <- memoizedFib (n - 1)
      return $ a + b

    memoizedFib :: Integer -> S Integer
    memoizedFib = memoize fib

-- fixed memoize
memoize :: (Eq a, Ord a) => (a -> State (Map a b) b) -> a -> State (Map a b) b
memoize f x = do
  computed <- get
  case Map.lookup x computed of
    Just result ->
      return result
    Nothing -> do
      result <- f x
      --m <- get
      --put $ Map.insert x result m
      modify (Map.insert x result)
      return result


-- swap n-2/n-1 so n-1 is first  -- even more broken
fibM2 :: Integer -> Integer
fibM2 = runS . fib
  where
    fib :: Integer -> S Integer
    fib 0 = return 0
    fib 1 = return 1
    fib n = do
      a <- memoizedFib (n - 1)
      b <- memoizedFib (n - 2)
      return $ a + b

    memoizedFib :: Integer -> S Integer
    memoizedFib = memoize fib

    -- broken memoize
    memoize :: (Eq a, Ord a) => (a -> State (Map a b) b) -> a -> State (Map a b) b
    memoize f x = do
      computed <- get
      case Map.lookup x computed of
        Just result ->
          return result
        Nothing -> do
          result <- f x
          put $ Map.insert x result computed
          return result


-- original version from Samir...
fibM1 :: Integer -> Integer
fibM1 = runS . fib
  where
    fib :: Integer -> S Integer
    fib 0 = return 0
    fib 1 = return 1
    fib n = do
      a <- memoizedFib (n - 2)
      b <- memoizedFib (n - 1)
      return $ a + b

    memoizedFib :: Integer -> S Integer
    memoizedFib = memoize fib

    -- broken memoize
    memoize :: (Eq a, Ord a) => (a -> State (Map a b) b) -> a -> State (Map a b) b
    memoize f x = do
      computed <- get
      case Map.lookup x computed of
        Just result ->
          return result
        Nothing -> do
          result <- f x
          put $ Map.insert x result computed
          return result


-- State monad/Map based memoization...
type S a = State (Map Integer Integer) a
runS :: S a -> a
runS s = evalState s Map.empty


-- vanilla
fibV :: Integer -> Integer
fibV 0 = 0
fibV 1 = 1
fibV n = fibV (n - 2) + fibV (n - 1)



-- what value of N is reached in 1/10 second
test :: String -> (Integer -> Integer) -> IO ()
test tag f = do
  let tenth = Nanos (gig `div` 10)
  start <- getTime Monotonic
  let
    loop :: Integer -> IO ()
    loop n = do
      let !_res = f n
      --print (tag,n,_res)
      now <- getTime Monotonic
      if timeSpec2nanos(now-start) < tenth then loop (n+1) else do
        --print (tag,n)
        printf "%s: N = %d (#decimal digits = %d)\n"
          (rjust 20 tag) n (length (show _res))
        pure ()
  loop 0
    where
      rjust n s = replicate (n - length s) ' ' ++ s



newtype Nanos = Nanos Int64 deriving (Eq,Ord,Num)

timeSpec2nanos :: TimeSpec -> Nanos
timeSpec2nanos = \case
  TimeSpec{sec,nsec} -> Nanos (gig * sec + nsec)

instance Show Nanos where
  show (Nanos i) = printf "%.03fs" dub
    where dub :: Double = fromIntegral i / fromIntegral gig

gig :: Int64
gig = 1_000_000_000
