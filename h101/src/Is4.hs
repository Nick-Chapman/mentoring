module Is4 (main) where

import Text.Printf (printf)
import Data.Word (Word8)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (ap,liftM)

type Byte = Word8

main :: IO ()
main = do
  print "*Is4*"
  runTests

-- maps
look :: (Ord k, Show k) => String -> Map k v -> k -> v
look tag m k = maybe err id (Map.lookup k m) where err = error (show ("look",tag,k))

extend :: Ord k => Map k v -> k -> v -> Map k v
extend m k v = Map.insert k v m

-- list diff
(\\) :: [a] -> [a] -> [a]
(\\) = undefined

----------------------------------------------------------------------
-- tests

runTests :: IO ()
runTests = do

  let a = "a"
  let x = "x"
  let y = "y"
  let z = "z"
  let z2 = "z2"

  let env :: Env = Map.fromList [(a,RegA), (x,RegX), (y,RegY), (z,ZeroPage 1), (z2,ZeroPage 2)]
  let ee :: EvalEnv = Map.fromList [(a,13),(x,42),(y,101),(z,19),(z2,28)]

  let t1 = "t1"
  let t2 = "t2"
  let t3 = "t3"
  let t4 = "t4"
  let t5 = "t5"

  let
    examples =
      [ Return (Num 77)

      , Return (Var a)
      , Return (Var x)
      , Return (Var y)
      , Return (Var z)

      , Let t1 (Add (Var a) (Num 1)) (Return (Var t1))
      , Let t1 (Add (Var x) (Num 1)) (Return (Var t1))
      , Let t1 (Add (Var y) (Num 1)) (Return (Var t1))
      , Let t1 (Add (Var z) (Num 1)) (Return (Var t1))

      , Let t1 (Add (Var a) (Var a)) (Return (Var t1))
      , Let t1 (Add (Var a) (Var x)) (Return (Var t1))
      , Let t1 (Add (Var a) (Var y)) (Return (Var t1))
      , Let t1 (Add (Var a) (Var z)) (Return (Var t1))

      , Let t1 (Add (Var x) (Var a)) (Return (Var t1))
      , Let t1 (Add (Var x) (Var x)) (Return (Var t1))
      , Let t1 (Add (Var x) (Var y)) (Return (Var t1))
      , Let t1 (Add (Var x) (Var z)) (Return (Var t1))

      , Let t1 (Add (Var y) (Var a)) (Return (Var t1))
      , Let t1 (Add (Var y) (Var x)) (Return (Var t1))
      , Let t1 (Add (Var y) (Var y)) (Return (Var t1))
      , Let t1 (Add (Var y) (Var z)) (Return (Var t1))

      , Let t1 (Add (Var z) (Var a)) (Return (Var t1))
      , Let t1 (Add (Var z) (Var x)) (Return (Var t1))
      , Let t1 (Add (Var z) (Var y)) (Return (Var t1))
      , Let t1 (Add (Var z) (Var z)) (Return (Var t1))

      , Let t1 (Add (Var a) (Num 2)) (Return (Var t1))
      , Let t1 (Add (Var x) (Num 2)) (Return (Var t1))
      , Let t1 (Add (Var y) (Num 2)) (Return (Var t1))
      , Let t1 (Add (Var z) (Num 2)) (Return (Var t1))

      , Let t1 (Add (Num 17) (Num 19)) (Return (Var t1))
      , Let t1 (Number 17) (Return (Var t1))

      , Let t1 (Add (Var z) (Num 1))
        (Let t2 (Add (Var z2) (Num 2))
          (Let t3 (Add (Var t1) (Num 3))
           (Let t4 (Add (Var t2) (Num 1))
             (Let t5 (Add (Var t4) (Var t1))
              (Return (Var t5))))))

      ]

  printf "(compile)env = %s\n" (show env)
  printf "(eval)env = %s\n" (show ee)
  let ms0 :: MachineState = initMS env ee
  printf "ms0 = %s\n" (show ms0)

  mapM_ (run1 env ee ms0) examples


run1 :: Env -> EvalEnv -> MachineState -> Exp -> IO ()
run1 env ee ms0 example = do
  printf "\nexample = %s\n" (show example)
  let eres = eval ee example
  printf "eres = %d\n" eres

  let temps1 = Temps [ZeroPage n | n <- [7..19]]
  let rs :: [Res] = runAsm temps1 (compile env example)
  let num = length rs
  printf "#results = %d\n" num
  if num == 0 then error "#results==0" else pure ()
  let
    prRes :: Res -> IO ()
    prRes (code,loc) = do
      let mres = emulate ms0 (code,loc)
      let same = (mres == eres)
      let ok :: String = if same then " {ok}" else printf " {FAIL: different: %d}" mres
      printf "%s --> %s%s\n" (show code) (show loc) ok
      if not same then error "not ok" else pure ()

  mapM_ prRes rs


----------------------------------------------------------------------
-- emulate

data MachineState = MS { m :: Map Loc Byte } -- flags will go in here also
  deriving Show

initMS :: Env -> EvalEnv -> MachineState
initMS env ee = do
  let m = Map.fromList [ (loc,look "initMS" ee var) | (var,loc) <- Map.toList env ]
  MS m

emulate :: MachineState -> (Code,Loc) -> Byte
emulate ms0 (code,locFinal) = steps ms0 code
  where
    steps ms = \case
      [] -> get_loc ms locFinal
      i:is -> steps (step ms i) is

get_loc :: MachineState -> Loc -> Byte
get_loc MS{m} = \case
  --Literal v -> v
  loc -> look "get_loc" m loc

step :: MachineState -> Instruction -> MachineState
step ms@MS{m} = do
  let get = get_loc ms
  let zp z = get (ZeroPage z)
  let a = RegA
  let x = RegX
  let y = RegY
  let up k v = ms { m = extend m k v }
  \case
    Clc -> ms -- not tracking flags yet
    Asl_A -> up a (2 * get a)
    Tax -> up x (get a)
    Tay -> up y (get a)
    Txa -> up a (get x)
    Tya -> up a (get y)
    Adc_L v -> up a (get a + v)
    Adc_Z z -> up a (get a + zp z)
    Lda_L v -> up a v
    Lda_Z z -> up a (zp z)
    Inxy X -> up x (get x + 1)
    Inxy Y -> up y (get y + 1)
    Ldxy_Z X z -> up x (zp z)
    Ldxy_Z Y z -> up y (zp z)
    Ldxy_L X v -> up x v
    Ldxy_L Y v -> up y v
    Inc_Z z -> up (ZeroPage z) (zp z + 1)
    Sta_Z z -> up (ZeroPage z) (get a)
    Stxy X z -> up (ZeroPage z) (get x)
    Stxy Y z -> up (ZeroPage z) (get y)


----------------------------------------------------------------------
-- eval

type EvalEnv = Map Var8 Byte

eval :: EvalEnv -> Exp -> Byte
eval ee = \case
  Return a -> atom a
  Let x rhs exp -> eval (extend ee x (oper rhs)) exp

  where
    oper :: Oper8 -> Byte
    oper = \case
      Add a1 a2 -> atom a1 + atom a2
      Double a -> 2 * atom a
      Number n -> n

    atom :: Atom8 -> Byte
    atom = \case
      Var x -> look "eval" ee x
      Num b -> b



----------------------------------------------------------------------
-- Asm

--failAsm :: Asm a
--failAsm = Alts []

data Asm a where
    Ret :: a -> Asm a
    Bind :: Asm a -> (a -> Asm b) -> Asm b
    Emit :: Instruction -> Asm ()
    Alts :: [Asm a] -> Asm a
    WithFreshTemp :: (Loc -> Asm a) -> Asm a

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

type Res = (Code,Loc)
type Code = [Instruction]

runAsm :: Temps -> Asm Loc -> [Res]
runAsm down asm0 = loop down asm0
  where
    loop :: Temps -> Asm a -> [(Code,a)]
    loop down = \case
      Ret x -> [([],x)]
      Bind m f -> [ (code1++code2,b) | (code1,a) <- loop down m, (code2,b) <- loop down (f a) ]
      Emit i -> [([i],())]
      Alts ms -> [ res | m <- ms, res <- loop down m ]
      WithFreshTemp k -> do
        let (loc,down') = nextTemp down
        loop down' (k loc)

data Temps = Temps [Loc]

nextTemp :: Temps -> (Loc,Temps)
nextTemp = \case
  Temps [] -> error "run out of temps"
  Temps (loc:locs) -> (loc,Temps locs)



----------------------------------------------------------------------
-- language

data Exp
  = Return Atom8
  | Let Var8 Oper8 Exp

type Var8 = String
data Oper8
  = Add Atom8 Atom8
  | Double Atom8 -- TODO: exmple using this op
  | Number Byte -- step towards general exp language?
data Atom8 = Var Var8 | Num Byte

instance Show Exp where
  show = \case
    Let x rhs body -> printf "let %s = %s in %s" x (show rhs) (show body)
    Return a -> show a

instance Show Oper8 where
  show = \case
    Add a1 a2 -> printf "(%s + %s)" (show a1) (show a2)
    Double a -> printf "(%s << 1)" (show a)
    Number n -> show n

instance Show Atom8 where
  show = \case
    Var x -> x
    Num n -> show n


----------------------------------------------------------------------
-- instruction

data Instruction
  = Clc
  | Asl_A
  | Tax | Tay | Txa | Tya
  | Adc_Z Byte | Adc_L Byte
  | Lda_Z Byte | Lda_L Byte
  | Inxy XY | Ldxy_Z XY Byte | Ldxy_L XY Byte
  | Inc_Z Byte
  | Sta_Z Byte
  | Stxy XY Byte

-- capture commonality betweem two index regs
data XY = X | Y deriving (Eq,Ord)

instance Show Instruction where
  show = \case
    Clc -> "clc"
    Asl_A -> "asl a"
    Tax -> "tax"
    Tay -> "tay"
    Txa -> "txa"
    Tya -> "tya"
    Adc_L v -> printf "adc #%d" v
    Adc_Z z -> printf "adc %d" z
    Lda_L v -> printf "lda #%d" v
    Lda_Z z -> printf "lda %d" z
    Inxy xy -> printf "in%s" (show xy)
    Ldxy_Z xy z -> printf "ld%s %d" (show xy) z
    Ldxy_L xy v -> printf "ld%s #%d" (show xy) v
    Inc_Z z -> printf "inc %d" z
    Sta_Z z -> printf "sta %d" z
    Stxy xy z -> printf "st%s %d" (show xy) z

instance Show XY where
  show = \case X -> "x"; Y -> "y"


----------------------------------------------------------------------
-- compilation; code generation; instruction selection

data Arg = Lit Byte | Loc Loc

data Loc = RegA | RegX | RegY | ZeroPage Byte
  deriving (Eq,Ord,Show)

compile :: Env -> Exp -> Asm Loc
compile env exp = do
  target <- Alts [ pure t | t <- [RegA,RegX,RegY] ] -- Also ZeroPage 0 ??
  compile1 env target exp
  pure target

compile1 :: Env -> Loc -> Exp -> Asm ()
compile1 env target = \case
  Return x -> copy (locate env x) target
  Let x rhs body -> do
    withSelectTarget env body $ \rhsT -> do
      generate env rhsT rhs
      let env' = extend env x rhsT
      compile1 env' target body

withSelectTarget :: Env -> Exp -> (Loc -> Asm a) -> Asm a
withSelectTarget env body k = do
  let needed = neededLocs env body
  let targets = [RegA,RegX,RegY] \\ needed
  WithFreshTemp $ \tmp -> do
    Alts [ k t | t <- targets ++ [tmp] ]

locate :: Env -> Atom8 -> Arg
locate env = \case
  Var x -> Loc (look "locate" env x)
  Num b -> Lit b

----------------------------------------------------------------------
-- compile-time env: for each var, where might it be at runtime

type Env = Map Var8 Loc

neededLocs :: Env -> Exp -> [Loc]
neededLocs = undefined free

free :: Exp -> [Loc]
free = undefined

----------------------------------------------------------------------
-- instruction selection

copy :: Arg -> Loc -> Asm ()
copy = undefined
--copy _ _ = pure ()

generate :: Env -> Loc -> Oper8 -> Asm ()
generate = undefined
--generate _ _ _ = pure ()
