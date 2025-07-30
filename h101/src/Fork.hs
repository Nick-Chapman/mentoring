module Fork (main) where

import System.Posix.Process (forkProcess)
import System.Process (Pid,getCurrentPid)
import System.IO (hFlush,stdout)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
  putStrLn "MAIN"
  let _ = hFlush stdout
  pid <- forkProcess child
  parent
  wait pid
  putStrLn "JOINED"

wait :: Pid -> IO ()
wait _ = pure () -- TODO: this is what we want!

parent :: IO ()
parent = do
  threadDelay 100000
  message "parent1"
  threadDelay 200000
  message "parent2"

child :: IO ()
child = do
  threadDelay 200000
  message "child1"
  threadDelay 200000
  message "child2"

message :: String -> IO ()
message str = do
  pid <- getCurrentPid
  putStrLn ("[" ++ show pid ++ "]-" ++ str)

