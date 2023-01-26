module Top  where

import qualified Interpreter

main :: IO ()
main = do
  putStrLn "*h101*"
  Interpreter.main
