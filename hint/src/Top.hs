module Top (main) where

import Parser (parse)
import Pretty (pretty)
import Evaluator (execute)

main :: IO ()
main = do
  _str <- readFile "../cint/example.prog"
  let str = " main = (100 - 1) + (1000 - 200) + 770000  "
  case parse str of
    Left err -> do
      print "**parse failure**"
      print err
    Right prog -> do
      print "**parse ok**"
      mapM_ putStrLn (pretty prog)
      let v = execute prog
      print v
