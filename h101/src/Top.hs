module Top  where

import qualified Interpreter

main :: IO ()
main = do
  putStrLn "*h101*"
  Interpreter.main
  --print prog

{-
prog :: Int
prog = do
  --let square x = x * x
  let inc x = x + 1

  --let compose f g x = f (g x)
  --let twice = \f -> \x -> f (f x)
  let thrice = \f -> \x -> f (f (f x))

  --let r = thrice (thrice inc) 0
  let r = thrice thrice inc 0

  r
-}
