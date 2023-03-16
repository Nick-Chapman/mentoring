
module OldBf(main) where

--import qualified Data.Ascii as Ascii (toChar)
--import Data.Word8 (Word8)
import Data.Char(chr)
import Data.List(intercalate)

main :: IO ()
main = do
  let progString = "++++++++++[->+>++++++++++<<]>>++++.---.+++++++..+++.<."
  --let progString = ".+."
  --let progString = "++[->+<]."

  let f = "/home/nic/code/bf/b/fib.b"
  let _f = "/home/nic/code/bf/b/mandelbrot.b"
  _progString <- readFile f

  --print ("progString=", progString)
  let prog = parse progString
  --print ("prog=", prog)

  let _xs = debugRun prog
  let
    pr (s,Nothing) = putStrLn (show s)
    pr (s,Just mes) = putStrLn (show s ++ " --> " ++ mes)
  let _ = mapM_ pr _xs

  let _output = evaluateOutput prog
  putStr _output

  pure ()


parse :: String -> Prog
parse cs =
  Prog [ x
       | c <- cs
       , x <- case parseChar c of Nothing -> []; Just x -> [x]
       ]

parseChar :: Char -> Maybe Op
parseChar = \case
  '+' -> Just Plus
  '-' -> Just Minus
  '[' -> Just Lsquare
  ']' -> Just Rsquare
  '<' -> Just Langle
  '>' -> Just Rangle
  '.' -> Just Dot
  ',' -> Just Comma
  _ -> Nothing --error (show ("parseChar",c))

data Prog = Prog [Op]

instance Show Prog where
  show (Prog ops) =
    "\"" ++ concat (map show ops) ++ "\""


data Op = Plus | Minus | Langle | Rangle | Lsquare | Rsquare | Dot | Comma

instance Show Op where
  show = (\x -> [x])  . \case
    Dot -> '.'
    Comma -> ','
    Plus -> '+'
    Minus -> '-'
    Langle -> '<'
    Rangle -> '>'
    Lsquare -> '['
    Rsquare -> ']'


debugRun :: Prog -> [(State,Maybe String)]
debugRun prog = loop (state0 prog)
  where
    loop :: State -> [(State,Maybe String)]
    loop s =
      case step s of
        Nothing -> []
        Just (optMes,s2) -> (s,optMes) : loop s2



evaluateOutput :: Prog -> String
evaluateOutput prog = loop (state0 prog)
  where
    loop :: State -> String
    loop s =
      case step s of
        Nothing -> ""
        Just (Nothing,s) -> loop s
        Just (Just mes,s) -> mes ++ loop s


data State = State { prog :: Prog, mem :: Mem }
data Mem = Mem [Cell] Cell [Cell]
data Cell = Cell Int

instance Show State where
  show State { prog, mem } =
    show (abbrev prog) ++ " -- " ++ show mem

instance Show Mem where
  show (Mem l c r) =
    "..."
    ++ intercalate " " (map show (reverse l))
    ++ " (" ++ show c ++ ") "
    ++ intercalate " " (map show r) ++ "..."

instance Show Cell where
  show (Cell x) = show x

abbrev :: Prog -> Prog
abbrev (Prog ops) = Prog (take 5 ops)

state0 :: Prog -> State
state0 prog = State { prog, mem = mem0 }
  where
    mem0 :: Mem
    mem0 = Mem [] cell0 []

cell0 :: Cell
cell0 = Cell 0


step :: State -> Maybe (Maybe String,  State)
step s@State{prog=Prog ops,mem} = case ops of
  [] -> Nothing
  op:ops ->
    let
      updateMem :: Mem -> Maybe (Maybe String,  State)
      updateMem mem =
        Just (Nothing, s { prog = Prog ops, mem = mem })
      updateOps :: [Op] -> Maybe (Maybe String,  State)
      updateOps ops =
        Just (Nothing, s { prog = Prog ops })
    in
    case op of
      Dot -> Just (Just (charAtPointer mem) , s { prog = Prog ops })
      Comma -> error "comma not supported yet"
      Plus -> updateMem (doPlus mem)
      Minus -> updateMem (doMinus mem)
      Langle -> updateMem (doLeft mem)
      Rangle -> updateMem (doRight mem)
      Lsquare ->
        if isZeroAtPointer mem
        then updateOps (dropUntilMatchingRsquare 0 ops)
        else updateOps (takeUntilMatchingRsquare 0 ops ++ [Lsquare] ++ ops)
      Rsquare -> error "unexpected ]"


-- foo[hmm]bar]more --> more
takeUntilMatchingRsquare :: Int -> [Op] -> [Op]
takeUntilMatchingRsquare n = \case
  [] -> error "no-matching ]"
  x@Rsquare:xs -> if n>0 then x:takeUntilMatchingRsquare (n-1) xs else []
  x@Lsquare:xs -> x:takeUntilMatchingRsquare (n+1) xs
  x:xs -> x:takeUntilMatchingRsquare n xs

dropUntilMatchingRsquare :: Int -> [Op] -> [Op]
dropUntilMatchingRsquare n = \case
  [] -> error "no-matching ]"
  Rsquare:xs -> if n>0 then dropUntilMatchingRsquare (n-1) xs else xs
  Lsquare:xs -> dropUntilMatchingRsquare (n+1) xs
  _:xs -> dropUntilMatchingRsquare n xs


doPlus :: Mem -> Mem
doPlus (Mem l (Cell x) r) = Mem l (Cell (x+1)) r

doMinus :: Mem -> Mem
doMinus (Mem l (Cell x) r) = Mem l (Cell (x-1)) r

doRight :: Mem -> Mem
doRight (Mem l c r) = case r of
  [] -> Mem (c:l) cell0 []
  r1:r -> Mem (c:l) r1 r

doLeft :: Mem -> Mem
doLeft (Mem l c r) = case l of
  [] -> error "moved left from start of mem tape"
  l1:l -> Mem l l1 (c:r)

charAtPointer :: Mem -> String
charAtPointer (Mem _ (Cell x) _) = do
  -- Ascii.toChar x
  [chr x]
  --"Output: n=" ++ show x ++ ", c=[" ++ show (chr x) ++ "]\n"

isZeroAtPointer :: Mem -> Bool
isZeroAtPointer (Mem _ (Cell x) _) = (x==0)
