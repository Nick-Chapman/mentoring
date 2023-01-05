module Top (main) where

import Parser (parse)
import Pretty (pretty)
import Evaluator (execute)

main :: IO ()
main = do
  putStrLn "*hint*"
  _str <- readFile "../cint/example.prog"
  let str = " main = 12 + 34 + 56 "
  case parse str of
    Left err -> print err
    Right prog -> do
      mapM_ putStrLn (pretty prog)
      let v = execute prog
      print v
