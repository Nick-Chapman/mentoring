
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

main :: IO ()
main = do
  taOrig <- enableRawMode
  let
    loop state = do
      display state
      key <- readKey
      pure (processKey state key) >>= \case
        Nothing -> do
          putOut (clearEntireScreen ++ home ++ "Quitting...\r\n")
          disableRawMode taOrig
          pure ()
        Just state -> do
          loop (incFrames state)
            where incFrames state@State{numFrames=i} = state {numFrames = i+1}

  loop state0

display :: State -> IO ()
display state = do
  putOut (composit state)

composit :: State -> String
composit State{screenRows,numFrames,cx,cy} =
  home ++
  concat
  [ line ++ clearRestOfLine ++ (if r < screenRows-1 then "\r\n" else "")
  | r <- [0..screenRows-1]
  , let
      line =
        if r==welcomeRow
        then printf "~ [#frames:%d, cx=%d, cy=%d]" numFrames cx cy
        else "~"
  ]
  ++ moveCursorTo cx cy
  where
    welcomeRow = 3

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

keyBinding :: Key -> Action
keyBinding = \case
  Key 'a' -> GoLeft
  Key 'd' -> GoRight
  Key 'w' -> GoUp
  Key 's' -> GoDown
  k -> IgnoreKey k

data Action
  = GoLeft
  | GoRight
  | GoUp
  | GoDown
  | IgnoreKey Key

processAction :: State -> Action -> State
processAction s@State{cx,cy,screenRows,screenCols} = \case
  GoLeft -> s { cx = bound (0,screenCols) (cx - 1) }
  GoRight -> s { cx = bound (0,screenCols) (cx + 1) }
  GoUp -> s { cy = bound (0,screenRows) (cy - 1) }
  GoDown -> s { cy = bound (0,screenRows) (cy + 1) }
  IgnoreKey{} -> s

bound :: (Int,Int) -> Int -> Int
bound (lo,hi) x = min (hi-1) (max lo x)

data State = State
  { screenCols :: Int
  , screenRows :: Int
  , lastKeys :: [Key]
  , numFrames :: Int
  , cx :: Int
  , cy :: Int
  }

state0 :: State
state0 = State
  { screenRows = 10
  , screenCols = 40
  , lastKeys = []
  , numFrames = 0
  , cx = 0
  , cy = 0
  }

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
