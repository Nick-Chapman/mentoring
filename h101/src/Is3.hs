module Is3 (main) where

import Text.Printf (printf)
import Data.Word (Word8)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (ap,liftM)
--import Control.Monad.Fix (MonadFix,mfix)

type Byte = Word8

main :: IO ()
main = do
  print "*Is3*"
  runTests

-- maps
look :: (Ord k, Show k) => String -> Map k v -> k -> v
look tag m k = maybe err id (Map.lookup k m) where err = error (show ("look",tag,k))

extend :: Ord k => Map k v -> k -> v -> Map k v
extend m k v = Map.insert k v m


----------------------------------------------------------------------
-- tests

runTests :: IO ()
runTests = do

  let a = "a"
  let x = "x"
  let y = "y"
  let z = "z"
  let z2 = "z2"

  let env = Map.fromList [(a,RegA), (x,RegXY X), (y,RegXY Y), (z,ZeroPage 1), (z2,ZeroPage 2)]
  let ee = Map.fromList [(a,13),(x,42),(y,101),(z,19),(z2,28)]

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
-- compilation; code generation; instruction selection


-- runtime locations
data Loc = RegA | RegXY XY | ZeroPage Byte | Literal Byte deriving (Eq,Ord)
-- wondering if having Literal as a kind of Loc is a bad idea

instance Show Loc where
  show = \case
    RegA -> "A"
    RegXY X -> "X"
    RegXY Y -> "Y"
    ZeroPage z -> printf "Zp%d" z
    Literal v -> printf "#%d" v


-- compile-time env: for each var, where might it be at runtime
type Env = Map Var8 Loc


compile :: Env -> Exp -> Asm Loc
compile env = \case
  Return x -> pure (locate env x)
  Let x rhs body -> do -- TODO: cant get mdo working :(
    loc <- generate env rhs
    WithFreshTemp $ \loc2 -> do
      copy (loc,loc2)
      let env' = extend env x loc2
      compile env' body


-- TODO: version of compile which doesn't generate freshtemp with such free abandom
{-_compile1 :: Env -> Exp -> Asm Loc
_compile1 = undefined shiftMaybe restrict free

shiftMaybe :: Loc -> Env -> Env
shiftMaybe = undefined

restrict :: [Loc] -> Env -> Env
restrict = undefined

free :: Exp -> [Loc]
free = undefined
-}


locate :: Env -> Atom8 -> Loc
locate ec = \case
  Var x -> look "locate" ec x
  Num b -> Literal b

generate :: Env -> Oper8 -> Asm Loc
generate env oper = Alts [ g env oper | g <- generators ]

generators :: [Env -> Oper8 -> Asm Loc]
generators =
  [ genIncXY X
  , genIncXY Y
  , genAdc
  , genInc
  , number
  ]

number :: Env -> Oper8 -> Asm Loc
number _ = \case
  Number n -> pure (Literal n)
  _ -> failAsm


genIncXY :: XY -> Env -> Oper8 -> Asm Loc
genIncXY xy ec oper8 = do
  case oper8 of
    Number{} -> failAsm
    Double{} -> failAsm
    Add pA qA -> do
      let pL = locate ec pA
      let qL = locate ec qA
      case (pL,qL) of
        (q, Literal 1) -> inc q
        (Literal 1, q) -> inc q
        _ -> failAsm
        where
          inc :: Loc -> Asm Loc
          inc q = do
            loadXY xy q
            Emit (Inxy xy)
            return (RegXY xy)

genInc :: Env -> Oper8 -> Asm Loc
genInc env oper8 = do
  case oper8 of
    Number{} -> failAsm
    Double{} -> failAsm
    Add pA qA -> do
      let pL = locate env pA
      let qL = locate env qA
      case (pL,qL) of
        (ZeroPage b, Literal 1) -> inc b
        (Literal 1, ZeroPage b) -> inc b
        _ -> failAsm
        where
          inc :: Byte -> Asm Loc
          inc b = do
            Emit (Inc_Z b)
            return (ZeroPage b)


genAdc :: Env -> Oper8 -> Asm Loc
genAdc env = \case
  Number{} -> failAsm
  Double{} -> failAsm
  Add pA qA -> do
    let pL = locate env pA
    let qL = locate env qA
    case (pL,qL) of
      (RegA,loc) -> do addIntoAcc loc; pure RegA
      (loc,RegA) -> do addIntoAcc loc; pure RegA
      (loc1,loc2) -> do loadAcc loc1; addIntoAcc loc2; pure RegA

----------------------------------------------------------------------
-- asm helpers

addIntoAcc :: Loc -> Asm ()
addIntoAcc = \case
  RegA -> Emit Asl_A
  ZeroPage z -> do clc; adc_Z z
  Literal v -> do clc; adc_L v
  --RegXY{} -> failAsm
  loc@(RegXY{}) -> do
    WithFreshTemp $ \temp -> do
      copy (loc,temp)
      addIntoAcc temp


loadAcc :: Loc -> Asm ()
loadAcc = \case
  RegXY X -> Emit Txa
  RegXY Y -> Emit Tya
  RegA -> undefined -- never called; how might it?
  ZeroPage b -> Emit (Lda_Z b)
  Literal b -> Emit (Lda_L b)

loadXY :: XY -> Loc -> Asm ()
loadXY xy = \case
  RegXY xy2 -> copyXY (xy2,xy)
  RegA -> taxy xy
  ZeroPage b -> Emit (Ldxy_Z xy b)
  Literal b -> Emit (Ldxy_L xy b)

copy :: (Loc,Loc) -> Asm ()  -- source --> target
copy = \case
  (_,Literal{}) -> error "copy targeting literal makes no sense" -- need better types!
  (loc,RegA) -> loadAcc loc
  (loc,RegXY xy) -> loadXY xy loc
  (loc,ZeroPage z) -> storeZeroPage z loc

storeZeroPage :: Byte -> Loc -> Asm ()
storeZeroPage z = \case
  RegA -> sta z
  RegXY xy -> Emit (Stxy xy z)
  ZeroPage z2 -> do lda_Z z2; sta z
  Literal v ->  -- ever called? -- needed if we allow numbers as RHSs in base lang
    -- explore having mutiple choices...
    Alts [ do lda_L v; sta z
         , do ldx_L v; stx z
         , do ldy_L v; sty z
         ]

copyXY :: (XY, XY) -> Asm ()
copyXY = \case
  (X,X) -> pure ()
  (X,Y) -> do txa; tay
  (Y,X) -> do tya; tax
  (Y,Y) -> pure ()

taxy :: XY -> Asm ()
taxy = \case
  X -> tax
  Y -> tay

clc,tax,tay,txa,tya :: Asm ()
clc = Emit Clc
tax = Emit Tax
tay = Emit Tay
txa = Emit Txa
tya = Emit Tya

lda_Z,lda_L,ldx_L,ldy_L,adc_Z,adc_L,sta,stx,sty :: Byte -> Asm ()

lda_Z z = Emit (Lda_Z z)
lda_L v = Emit (Lda_L v)
ldx_L v = Emit (Ldxy_L X v)
ldy_L v = Emit (Ldxy_L Y v)

adc_Z z = Emit (Adc_Z z)
adc_L v = Emit (Adc_L v)

sta z = Emit (Sta_Z z)
stx z = Emit (Stxy X z)
sty z = Emit (Stxy Y z)


----------------------------------------------------------------------
-- Asm

failAsm :: Asm a
failAsm = Alts []

data Asm a where
    --Mfix :: (a -> Asm a) -> Asm a
    Ret :: a -> Asm a
    Bind :: Asm a -> (a -> Asm b) -> Asm b
    Emit :: Instruction -> Asm ()
    Alts :: [Asm a] -> Asm a
    WithFreshTemp :: (Loc -> Asm a) -> Asm a

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind
--instance MonadFix Asm where mfix = Mfix

type Res = (Code,Loc)
type Code = [Instruction]

runAsm :: Temps -> Asm Loc -> [Res]
runAsm down asm0 = loop down asm0
  where
    loop :: Temps -> Asm a -> [(Code,a)]
    loop down = \case
      --Mfix f -> let results = [ res | (_,a) <- results, res <- loop down (f a) ] in results
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
  Literal v -> v
  loc -> look "get_loc" m loc

step :: MachineState -> Instruction -> MachineState
step ms@MS{m} = do
  let get = get_loc ms
  let zp z = get (ZeroPage z)
  let a = RegA
  let x = RegXY X
  let y = RegXY Y
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
