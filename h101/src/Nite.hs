
-- Following https://viewsourcecode.org/snaptoken/kilo ("Build Your Own Text Editor")

module Nite (main) where -- Nick's Terminal Editor

import Data.Char (ord,isControl)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (TerminalAttributes,TerminalMode(..),TerminalState(..),getTerminalAttributes,setTerminalAttributes,withoutMode)
import Text.Printf (printf)

import Control.Exception (try,SomeException)
import Data.ByteString qualified as BS (unpack)
import System.Posix.Terminal (withTime,withMinInput)
import Data.ByteString.Internal (w2c)
import System.Posix.IO.ByteString (fdRead)

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
  , buf :: Buf
  , debug :: Bool
  }


--readFile :: String -> IO Buf
--readFile = undefined

main :: IO ()
main = do
  buf <- Buf . lines <$> readFile "/tmp/example"
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
      , buf
      , debug = True
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
      getChar >>= \case
        'R' -> pure $ reverse ('R':acc)
        c -> loop (c:acc)

bottomRight :: String
bottomRight = "\x1b[999C\x1b[999B"


display :: State -> IO ()
display state = do
  putOut (composit state)

composit :: State -> String
composit state = ren (bufOfState state) state

seeRecentKey :: Int -> State -> String
seeRecentKey i State{frameNum,lastKeys} = do
  let (n,key) = head (drop i (zip [frameNum,frameNum-1..] lastKeys)) -- state why head is safe
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
    Key 'q' -> Nothing -- TODO: also hook on Ctr-C ?
    key -> do
      state <- pure $ state { lastKeys = key : lastKeys }
      let action = keyBinding key
      state <- pure (processAction state action)
      Just state

data Action
  = IgnoreKey Key
  | ToggleDebug
  | CurLeft
  | CurRight
  | CurUp
  | CurDown
  | OffLeft
  | OffRight
  | OffUp
  | OffDown
  | BeginingOfLine
  | EndOfLine
  | Home
  | End

-- TODO: handle PgUp/PgDn on number pad
-- TODO: handle Ctrl- and Shift- arrow and other special keys
keyBinding :: Key -> Action
keyBinding = \case
  EscapeOpenSquare (Just (Key 'A')) -> CurUp
  EscapeOpenSquare (Just (Key 'B')) -> CurDown
  EscapeOpenSquare (Just (Key 'C')) -> CurRight
  EscapeOpenSquare (Just (Key 'D')) -> CurLeft
  EscapeOpenSquare (Just (Key 'H')) -> Home
  EscapeOpenSquare (Just (Key 'F')) -> End
  --Key 'a' -> CurLeft
  --Key 'd' -> CurRight
  --Key 'w' -> CurUp
  --Key 's' -> CurDown
  Key 'j' -> OffLeft
  Key 'l' -> OffRight
  Key 'i' -> OffUp
  Key 'k' -> OffDown
  Key 'x' -> ToggleDebug
  Escape '<' -> Home
  Escape '>' -> End
  k@(Key _) -> IgnoreKey k
  k@(Escape _) -> IgnoreKey k
  k@(EscapeOpenSquare{}) -> IgnoreKey k

processAction :: State -> Action -> State
processAction s@State{offx,offy,curx,cury,screenRows,screenCols,debug} = \case
  CurLeft -> s { curx = bound (0,screenCols) (curx - 1) }
  CurRight -> s { curx = bound (0,screenCols) (curx + 1) }
  CurUp ->
    if cury == 0 then s { offy = max 0 (offy - 1) } else
      s { cury = bound (0,screenRows) (cury - 1) }
  CurDown ->
    if cury == screenRows - 1 then s { offy = offy + 1 } else
      s { cury = bound (0,screenRows) (cury + 1) }

  OffLeft -> s { offx = max 0 (offx - 1) }
  OffRight -> s { offx = max 0 (offx + 1) }
  OffUp -> s { offy = max 0 (offy - 1) }
  OffDown -> s { offy = max 0 (offy + 1) }
  IgnoreKey{} -> s
  ToggleDebug -> s { debug = not debug }
  BeginingOfLine -> undefined
  EndOfLine -> undefined
  Home -> s { offy = 0, cury = 0 }
  End -> s { offy = lengthBuf (bufOfState s) - screenRows
           , cury = screenRows - 1
           }

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
  ta <- pure $ withTime ta 2 -- allow fdRead to timeout (timeout specified in 1/10s of a second)
  ta <- pure $ withMinInput ta 0 -- no effect; default?
  setTerminalAttributes stdInput ta WhenFlushed
  pure taOrig

disableRawMode :: TerminalAttributes -> IO ()
disableRawMode ta = do
  setTerminalAttributes stdInput ta WhenFlushed

readKey :: IO Key
readKey = do
  readKeyOpt >>= \case
    Just k -> pure k
    Nothing -> readKey -- drop the read timeouts

readKeyOpt :: IO (Maybe Key) -- detect escape-[
readKeyOpt = do
  readKey1 >>= \case
    Just (Escape '[') -> do
      readKey1 >>= \case
        k -> pure (Just (EscapeOpenSquare k))
    Just k -> pure (Just k)
    Nothing -> pure Nothing


readKey1 :: IO (Maybe Key) -- detect escape
readKey1 = do
  readCharMaybe >>= \case
    Just '\x1b' -> do
      readCharMaybe >>= \case
        Just c -> pure (Just (Escape c))
        Nothing -> pure (Just (Key '\x1b'))
    Just c -> do
      pure (Just (Key c))
    Nothing -> do
      pure Nothing


readCharMaybe :: IO (Maybe Char)
readCharMaybe = do
  try (fdRead stdInput 1) >>= \case
    Left (_e::SomeException) -> pure Nothing
    Right bs ->
      case BS.unpack bs of
        [] -> undefined --pure Nothing
        [c] -> pure (Just (w2c c))
        _ -> error "getCharMaybe/>1"


data Key = Key Char | Escape Char | EscapeOpenSquare (Maybe Key)

instance Show Key where
  show = \case
    Escape c -> "(escape) " ++ show (Key c)
    EscapeOpenSquare k -> "(escape-[) " ++ show k
    Key c ->
      if isControl c
      then printf "%02x" (ord c)
      else printf "%02x ('%c')" (ord c) c

----------------------------------------------------------------------

data Buf = Buf [String]

lengthBuf :: Buf -> Int
lengthBuf (Buf xs) = length xs


bufOfState :: State -> Buf
bufOfState state@State{debug,buf,frameNum,curx,cury,screenRows,screenCols} =
  if debug then Buf square else buf
  where
    square =
      [ printf "[#frames:%d, curx=%d, cury=%d, col=%d, rows=%d]" frameNum curx cury screenCols screenRows ]
      ++
      [ seeRecentKey i state | i <- [0..frameNum-1] ]

ren :: Buf -> State -> String
ren (Buf lines) State{curx,cury,offx,offy,screenRows,screenCols} =
  home ++
  intercalate "\r\n" [ take screenCols (drop offx line) ++ clearRestOfLine
                     | line <- take screenRows (drop offy lines ++ repeat "")
                     ]
  ++ moveCursorTo curx cury
