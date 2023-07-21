module Top (main) where

import qualified Interpreter
import qualified Bytecode
import qualified MotivateStateMonad
import qualified OldBf
import qualified Bf
import qualified Sol
import qualified Cps

main :: IO ()
main = do
  --putStrLn "*h101*"
  let _ = Interpreter.main
  --print prog
  let _ = Bytecode.main
  let _ = MotivateStateMonad.main
  let _ = OldBf.main
  let _ = Bf.main
  let _ = Sol.main
  Cps.main
  pure ()


_prog :: Int
_prog = do
  let fact = fix (\fact -> \n -> if n < 1 then 1 else n * fact (n-1))
  1 + fact (2+3)

  where
    fix :: ((a->a) -> (a->a)) -> (a->a)
    --fix :: (g -> g) -> g
    fix openDefinition = do
      let func x = openDefinition func x
      func

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
