module Fork (main) where

import System.Posix.Process (forkProcess)
import System.Process (getCurrentPid)
import System.IO (hFlush,stdout)
import Control.Concurrent (threadDelay)
import WaitPid(waitpid)

main :: IO ()
main = do
  putStrLn "MAIN"
  let _ = hFlush stdout
  _one <- forkProcess childOne
  _two <- forkProcess childTwo
  parent
  waitpid _one
  waitpid _two
  putStrLn "JOINED"

parent :: IO ()
parent = do
  threadDelay 100000
  message "parent.1" -- .1
  threadDelay 200000
  message "parent.2" -- .3

childOne :: IO ()
childOne = do
  threadDelay 200000 -- .2
  message "one.1"
  threadDelay 300000 -- .5
  message "one.2"

childTwo :: IO ()
childTwo = do
  threadDelay 250000 -- .25
  message "two.1"
  threadDelay 200000 -- .45
  message "two.2"

message :: String -> IO ()
message str = do
  pid <- getCurrentPid
  putStrLn ("[" ++ show pid ++ "]-" ++ str)
