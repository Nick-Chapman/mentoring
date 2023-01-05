module Top (main) where

import Parser (parse)
import Pretty (pretty)
import Evaluator (eval)

main :: IO ()
main = do
  putStrLn "*hint*"
  str <- readFile "../cint/example.prog"
  case parse str of
    Left err -> print err
    Right prog -> do
      mapM_ putStrLn (pretty prog)
      let v = eval prog
      print v
