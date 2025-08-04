
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

main :: IO ()
main = do
  taOrig <- enableRawMode
  write "Nite..."
  let
    loop state = do
      key <- readKey
      pure (processKey state key) >>= \case
        Nothing -> do
          write "Quitting..."
          disableRawMode taOrig
          pure ()
        Just state -> do
          display state
          loop state

  loop state0

display :: State -> IO ()
display State{lastKey} = do
  write (show lastKey)

write :: String -> IO ()
write s = putStr (s ++ "\r\n")

processKey :: State -> Key -> Maybe State
processKey state key =
  case key of
    Key 'q' -> Nothing
    key -> Just $ state { lastKey = key }

data State = State { lastKey :: Key }

state0 :: State
state0 = State { lastKey = NoKey }

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
