
module Bf (main) where

import Data.Word (Word8)

main :: IO ()
main = do
  putStrLn "[BF mentoring]"
  let example :: String = "+++[->+>+<<]"
  go example

go :: String -> IO () -- TODO: a non-debug runner when we have "." (Dot)
go s = do
  let _ops = parse s -- TODO: reinstate when parser is done
  let ops = [Plus , Plus , Plus , Block [ Minus, Rarrow, Plus, Rarrow, Plus, Larrow, Larrow ]]
  let tapes = execFromStart ops
  mapM_ print (zip [0::Int ..] tapes)

data Op
  = Plus
  | Minus
  | Rarrow
  | Larrow
  -- | Lsquare | Rsquare - NO!
  | Block [Op]
  -- TODO: Dot (for printing)

parse :: String -> [Op]
parse = \case
  [] -> []
  x:xs ->
    case parse1 x of
      Nothing -> parse xs
      Just op -> op : parse xs
  where
    parse1 :: Char -> Maybe Op
    parse1 c = case c of
      '+' -> Just Plus
      '-' -> Just Minus
      '>' -> Just Rarrow
      -- TODO: < [ ] .
      _ -> Nothing

execFromStart :: [Op] -> [Tape]
execFromStart prog = exec prog tape0

exec :: [Op] -> Tape -> [Tape]
exec ops0 t = t : -- here is where we prepend the current tape(state) to the generated list
  case ops0 of
    [] -> []
    Minus:ops -> exec ops (doMinus t)
    Plus:ops -> exec ops (doPlus t)
    Rarrow:ops -> exec ops (doRight t)
    Larrow:ops -> exec ops (doLeft t)
    bl@(Block ops1) : ops2 ->
      if isZeroAtPoint t
      then exec ops2 t
      else exec (ops1 ++ bl : ops2) t


{-
foo[bar]qaz...
 o[bar]qaz...
  [bar]qaz...

       qaz...

  bar[bar]qaz...
-}

isZeroAtPoint :: Tape -> Bool
isZeroAtPoint Tape{point} = (point == 0)

doMinus :: Tape -> Tape
doMinus Tape{left,point=old,right} = Tape {left, point = old-1, right}

doPlus :: Tape -> Tape
doPlus Tape{left,point=old,right} = Tape {left, point = old+1, right}

doRight :: Tape -> Tape
doRight Tape{left,point,right} =
  case right of
    []       -> Tape { left = point : left , point = 0 , right }
    r1:right -> Tape { left = point : left , point = r1, right }

doLeft :: Tape -> Tape
doLeft Tape{left,point,right} =
  case left of
    []      -> Tape { left = []   , point = 0 , right = point : right }
    l1:left -> Tape { left = left , point = l1, right = point : right }

tape0 :: Tape
tape0 = Tape { left = [], point = 0, right = [] }

data Tape = Tape { left :: [Word8], point :: Word8, right :: [Word8] }
  deriving Show

--instance Show Tape where -- TODO: make a nice pretty printer
--  show = undefined
