
-- Following https://viewsourcecode.org/snaptoken/kilo ("Build Your Own Text Editor")

module Nite (main) where -- Nick's Terminal Editor

import Data.Char (ord,isControl)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (TerminalAttributes,TerminalMode(..),TerminalState(..),getTerminalAttributes,setTerminalAttributes,withoutMode)
import Text.Printf (printf)

--import Control.Exception (try,SomeException)
--import Data.ByteString qualified as BS (unpack)
--import System.Posix.Terminal (withTime,withMinInput)
--import Data.ByteString.Internal (w2c)
--import System.Posix.IO.ByteString (fdRead)

import System.IO qualified as IO (stdout)
import System.IO (hFlush,hPutStr)
import Prelude hiding (putStr)
import Par4 (int,lit,parse)
import Data.List(intercalate)


data State = State
  { screenCols :: Int
  , screenRows :: Int
  , lastKeys :: [Key]
  , frameNum :: Int
  , curx :: Int
  , cury :: Int
  , offx :: Int
  , offy :: Int
  }

main :: IO ()
main = do
  taOrig <- enableRawMode
  (screenCols,screenRows) <- discoverScreenSize
  putOut clearEntireScreen

  let
    reset message = do
      putOut (clearEntireScreen ++ home ++ message ++ "\r\n")
      disableRawMode taOrig

  let
    state0 :: State
    state0 = State
      { screenRows
      , screenCols
      , lastKeys = []
      , frameNum = 0
      , curx = 0
      , cury = 0
      , offx = 0
      , offy = 0
      }
  let
    loop state = do
      display state
      key <- readKey
      pure (processKey state key) >>= \case
        Nothing -> reset "Quitting!"
        Just state -> do
          loop (incFrames state)
            where incFrames state@State{frameNum=i} = state {frameNum = i+1}

  loop state0

discoverScreenSize :: IO (Int,Int)
discoverScreenSize = do
  putOut (bottomRight ++ "\x1b[6n]")
  s <- readPositionReport
  case parse "positionReport" report s of
    Right res -> pure res
    Left err -> error err
  where
    report = do
      lit '\x1b'
      lit '['
      y <- int
      lit ';'
      x <- int
      lit 'R'
      pure (x,y)

readPositionReport :: IO String
readPositionReport = loop ""
  where
    loop acc = do
      readKey >>= \case
        Key 'R' -> pure $ reverse ('R':acc)
        Key c -> loop (c:acc)
        NoKey -> undefined -- loop acc

bottomRight :: String
bottomRight = "\x1b[999C\x1b[999B"


display :: State -> IO ()
display state = do
  putOut (composit state)

seeRecentKey :: Int -> State -> String
seeRecentKey i State{frameNum,lastKeys} = do
  let (n,key) = head (drop i (zip [frameNum,frameNum-1..] (lastKeys ++ repeat NoKey)))
  printf "%d: %s" n (show key)

  where head = \case [] -> error "head"; x:_ -> x

clearEntireScreen :: String
clearEntireScreen = "\x1b[2J"

clearRestOfLine :: String
clearRestOfLine = "\x1b[K"

home :: String
home = "\x1b[H"

moveCursorTo :: Int -> Int -> String
moveCursorTo x y = printf "\x1b[%d;%dH" (y+1) (x+1)

putOut :: String -> IO () -- tutorial: changes mode to flush always?
putOut s = do
  hPutStr IO.stdout s
  hFlush IO.stdout
  pure ()

processKey :: State -> Key -> Maybe State
processKey state@State{lastKeys} key =
  case key of
    Key 'q' -> Nothing
    key -> do
      state <- pure $ state { lastKeys = key : lastKeys }
      let action = keyBinding key
      state <- pure (processAction state action)
      Just state

data Action
  = CurLeft
  | CurRight
  | CurUp
  | CurDown
  | OffLeft
  | OffRight
  | OffUp
  | OffDown
  | IgnoreKey Key

keyBinding :: Key -> Action
keyBinding = \case
  Key 'a' -> CurLeft
  Key 'd' -> CurRight
  Key 'w' -> CurUp
  Key 's' -> CurDown
  Key 'j' -> OffLeft
  Key 'l' -> OffRight
  Key 'i' -> OffUp
  Key 'k' -> OffDown
  k -> IgnoreKey k

processAction :: State -> Action -> State
processAction s@State{offx,offy,curx,cury,screenRows,screenCols} = \case
  CurLeft -> s { curx = bound (0,screenCols) (curx - 1) }
  CurRight -> s { curx = bound (0,screenCols) (curx + 1) }
  CurUp -> s { cury = bound (0,screenRows) (cury - 1) }
  CurDown -> s { cury = bound (0,screenRows) (cury + 1) }
  OffLeft -> s { offx = max 0 (offx - 1) }
  OffRight -> s { offx = max 0 (offx + 1) }
  OffUp -> s { offy = max 0 (offy - 1) }
  OffDown -> s { offy = max 0 (offy + 1) }
  IgnoreKey{} -> s

bound :: (Int,Int) -> Int -> Int
bound (lo,hi) x = min (hi-1) (max lo x)

enableRawMode :: IO TerminalAttributes
enableRawMode = do
  ta <- getTerminalAttributes stdInput
  let taOrig = ta
  ta <- pure $ foldl withoutMode ta
    [ EnableEcho
    , ProcessInput -- ICANON (no buffering)
    , KeyboardInterrupts -- ISIG (no Ctrl-C and Ctrl-Z)
    , StartStopOutput -- IXON (no Ctrl-S and Ctrl-Q)
    , ExtendedFunctions -- IEXTEN (no Ctrl-V) -- this was already off for me
    , MapCRtoLF -- ICRNL (fixes Ctrl-M and [Enter])
    , ProcessOutput -- OPOST (we must output carriage return (\r) before newline (\n))
    ]
--  ta <- pure $ withTime ta 1 -- allow readKey to return NoKey
--  ta <- pure $ withMinInput ta 0 -- no effect
  setTerminalAttributes stdInput ta WhenFlushed
  pure taOrig

disableRawMode :: TerminalAttributes -> IO ()
disableRawMode ta = do
  setTerminalAttributes stdInput ta WhenFlushed

readKey :: IO Key
readKey = do
  c <- getChar
  pure (Key c)

{-readKey :: IO Key
readKey = do
  try (fdRead stdInput 1) >>= \case
    Left (_e::SomeException) -> pure NoKey
    Right bs ->
      case BS.unpack bs of
        [] -> pure NoKey
        [c] -> pure (Key (w2c c))
        _ -> error "readKey/>1"-}

data Key = NoKey | Key Char

instance Show Key where
  show = \case
    NoKey -> "[NoKey]"
    Key c ->
      if isControl c
      then printf "%d" (ord c)
      else printf "%d ('%c')" (ord c) c

----------------------------------------------------------------------

data Buf = Buf [String] -- what is visible

composit :: State -> String
composit state = ren (see state) state

see :: State -> Buf
see state@State{frameNum,curx,cury,screenRows,screenCols} =
  Buf square
  where
    square =
      [ printf "[#frames:%d, curx=%d, cury=%d, col=%d, rows=%d]" frameNum curx cury screenCols screenRows ]
      ++
      [ seeRecentKey i state | i <- [0..frameNum] ]

ren :: Buf -> State -> String
ren (Buf lines) State{curx,cury,offx,offy,screenRows,screenCols} =
  home ++
  intercalate "\r\n" [ take screenCols (drop offx line) ++ clearRestOfLine
                     | line <- take screenRows (drop offy lines)
                     ]
  ++ moveCursorTo curx cury
