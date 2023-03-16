
module Bf (main) where

import Data.ByteString.Internal (w2c,c2w)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (isEOF)

main :: IO ()
main = do
  [path] <- getArgs
  progString <- readFile path
  let ops = parse progString
  let res = execFromStart ops
  seeResult res

--type Result = [Tape]
--seeResult tapes = mapM_ print (zip [0::Int ..] tapes)

data Result = Finish | Step Tape Result | Output Word8 Result
  | Input (Word8 -> Result)

seeResult :: Result -> IO ()
seeResult = loop 0
  where
    loop :: Int -> Result -> IO ()
    loop i = \case
      Finish -> putStrLn $ "** finished in " ++ show i ++ " steps."
      Step _tape res -> do
        --print ("Step",i,_tape)
        loop (i+1) res
      Output w res -> do
        --print ("output",w)
        putStr [w2c w]
        loop (i+1) res
      Input f -> do
        w <- do isEOF >>= \case
                  False -> c2w <$> getChar
                  True -> pure 0
        loop (i+1) (f w)

data Op
  = Plus
  | Minus
  | Rarrow
  | Larrow
  -- | Lsquare | Rsquare - NO!
  | Block [Op]
  | Dot
  | Comma


parse :: String -> [Op]
parse s = loop [] [] s
  where
    loop :: [Op] -> [[Op]] -> [Char] -> [Op]
    loop acc nest = \case
      [] ->
        case nest of
          [] -> reverse acc
          _:_ -> error "unclosed["
      x:xs -> do
        case x of
          '+' -> loop (Plus:acc) nest xs
          '-' -> loop (Minus:acc) nest xs
          '<' -> loop (Larrow:acc) nest xs
          '>' -> loop (Rarrow:acc) nest xs
          '.' -> loop (Dot:acc) nest xs
          ',' -> loop (Comma:acc) nest xs
          '[' -> loop [] (acc:nest) xs
          ']' ->
            case nest of
              [] -> error "unexpected]"
              acc1:nest -> loop (Block (reverse acc) : acc1) nest xs
          _ ->
            loop acc nest xs


execFromStart :: [Op] -> Result
execFromStart prog = exec prog tape0

exec :: [Op] -> Tape -> Result
exec ops0 t = Step t $ -- here is where we prepend the current tape(state) to the generated list
  case ops0 of
    [] -> Finish
    Minus:ops -> exec ops (doMinus t)
    Plus:ops -> exec ops (doPlus t)
    Rarrow:ops -> exec ops (doRight t)
    Larrow:ops -> exec ops (doLeft t)
    Dot:ops -> Output (wordAtPoint t) (exec ops t)
    Comma:ops -> Input (\w -> exec ops (setWordAtPoint w t))
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

setWordAtPoint :: Word8 -> Tape -> Tape
setWordAtPoint w Tape{left,right} = Tape {left,point=w,right}

wordAtPoint :: Tape -> Word8
wordAtPoint Tape{point} = point

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
