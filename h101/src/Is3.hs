module Is3 (main) where

import Text.Printf (printf)
import Data.Word (Word8)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (ap,liftM)

type Byte = Word8

main :: IO ()
main = do
  print "*Is3*"
  run1

-- maps
look :: (Ord k, Show k) => Map k v -> k -> v
look m k = maybe err id (Map.lookup k m) where err = error (show ("look",k))

extend :: Ord k => Map k v -> k -> v -> Map k v
extend m k v = Map.insert k v m


----------------------------------------------------------------------
-- language

data Exp
  = Return Atom8
  | Let Var8 Oper8 Exp

type Var8 = String
data Oper8 = Add Atom8 Atom8 | Double Atom8
data Atom8 = Var Var8 | Num Byte

instance Show Exp where
  show = \case
    Let x rhs body -> printf "let %s = %s in %s" x (show rhs) (show body)
    Return a -> show a

instance Show Oper8 where
  show = \case
    Add a1 a2 -> printf "(%s + %s)" (show a1) (show a2)
    Double a -> printf "(%s << 1)" (show a)

instance Show Atom8 where
  show = \case
    Var x -> x
    Num n -> show n


----------------------------------------------------------------------
-- running example

run1 :: IO ()
run1 = do
  let v1 = "v1"
  let v2 = "v2"
  let q1 = "q1"
  let q2 = "q2"
  let q3 = "q3"
  let q4 = "q4"
  let q5 = "q5"

  let
    example :: Exp
    example = do
      Let q1 (Add (Var v1) (Num 1))
        (Let q2 (Add (Var v2) (Num 2))
          (Let q3 (Add (Var q1) (Num 3))
            (Let q4 (Add (Var q2) (Num 1))
             (Let q5 (Add (Var q4) (Var q1))
              (Return (Var q5))))))

  let env = Map.fromList [(v1,ZeroPage 1), (v2,ZeroPage 2)]

  printf "example = %s\n" (show example)
  printf "env = %s\n" (show env)
  let temps = Temps [ZeroPage n | n <- [7..19]]
  let rs :: [Res] = runAsm temps (compile env example)
  printf "#results = %d\n" (length rs)
  mapM_ prRes rs
    where
      prRes :: Res -> IO ()
      prRes (code,loc) = do
        printf "%s --> %s\n" (show code) (show loc)


----------------------------------------------------------------------
-- compilation; code generation; instruction selection


-- runtime locations
data Loc = RegA | RegXY XY | ZeroPage Byte | Literal Byte deriving (Eq,Ord,Show)

-- compile-time env: for each var, where might it be at runtime
type Env = Map Var8 Loc


compile :: Env -> Exp -> Asm Loc
compile env = \case
  Return x -> pure (locate env x)
  Let x rhs body -> do
    loc <- generate env rhs
    loc2 <- FreshTemp
    move (loc,loc2)
    let env' = extend env x loc2
    compile env' body

locate :: Env -> Atom8 -> Loc
locate ec = \case
  Var x -> look ec x
  Num b -> Literal b

generate :: Env -> Oper8 -> Asm Loc
generate env oper = Alts [ g env oper | g <- generators ]

generators :: [Env -> Oper8 -> Asm Loc]
generators =
  [ genIncXY X
  , genIncXY Y
  , genAdc
  , genInc
  ]

genAdc :: Env -> Oper8 -> Asm Loc
genAdc env = \case
  Double{} -> failAsm
  Add pA qA -> do
    let pL = locate env pA
    let qL = locate env qA
    case (pL,qL) of
      (RegA,loc) -> do addIntoAcc loc; pure RegA
      (loc,RegA) -> do addIntoAcc loc; pure RegA
      (loc,ZeroPage b) -> do loadAcc loc; Emit Clc; Emit (Adc_Z b); pure RegA
      (loc,Literal b) -> do loadAcc loc; Emit Clc; Emit (Adc_L b); pure RegA
      _ -> undefined --failAsm -- more cases when things are in XY

genIncXY :: XY -> Env -> Oper8 -> Asm Loc
genIncXY xy ec oper8 = do
  case oper8 of
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


----------------------------------------------------------------------
-- asm helpers

addIntoAcc :: Loc -> Asm ()
addIntoAcc = \case
  RegXY{} -> undefined -- ever called?
  RegA -> undefined -- Emit Asl_a -- ever called?
  ZeroPage b -> do Emit Clc; Emit (Adc_Z b)
  Literal b -> do Emit Clc; Emit (Adc_L b)

loadAcc :: Loc -> Asm ()
loadAcc = \case
  RegXY X -> Emit Txa
  RegXY Y -> Emit Tya
  RegA -> undefined -- ever called?
  ZeroPage b -> Emit (Lda_Z b)
  Literal b -> Emit (Lda_L b)

loadXY :: XY -> Loc -> Asm ()
loadXY xy = \case
  RegXY xy2 -> moveXY (xy2,xy)
  RegA -> Emit Tax
  ZeroPage b -> Emit (Ldxy_Z xy b)
  Literal b -> Emit (Ldxy_L xy b)

move :: (Loc,Loc) -> Asm ()
move = \case
  (_,Literal{}) -> undefined -- this makes no sense. need better types
  (loc,RegA) -> loadAcc loc
  (loc,RegXY xy) -> loadXY xy loc
  (loc,ZeroPage z) -> storeZeroPage z loc

storeZeroPage :: Byte -> Loc -> Asm ()
storeZeroPage z = \case
  RegA -> sta_Z z
  RegXY xy -> Emit (Stxy_Z xy z)
  ZeroPage z2 -> do lda_Z z2; sta_Z z
  Literal{} -> undefined

moveXY :: (XY, XY) -> Asm () -- source to target
moveXY = \case
  (X,X) -> pure ()
  (X,Y) -> do txa; tay
  (Y,X) -> do tya; tax
  (Y,Y) -> pure ()

tax,tay,txa,tya :: Asm ()
tax = Emit Tax
tay = Emit Tay
txa = Emit Txa
tya = Emit Tya

sta_Z :: Byte -> Asm ()
sta_Z z = Emit (Sta_Z z)

lda_Z :: Byte -> Asm ()
lda_Z z = Emit (Lda_Z z)


----------------------------------------------------------------------
-- Asm

failAsm :: Asm a
failAsm = Alts []

data Asm a where
    Ret :: a -> Asm a
    Bind :: Asm a -> (a -> Asm b) -> Asm b
    Emit :: Instruction -> Asm ()
    Alts :: [Asm a] -> Asm a
    FreshTemp :: Asm Loc

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

type Res = (Code,Loc)
type Code = [Instruction]

runAsm :: Temps -> Asm Loc -> [Res]
runAsm temps asm0 = [ (code,loc) | (code,loc,_finalS) <- loop s0 asm0 ]
  where
    s0 = State { temps }
    loop :: State -> Asm a -> [(Code,a,State)]
    loop s = \case
      Ret x -> [([],x,s)]
      Bind m f -> [ (code1++code2,b,s) | (code1,a,s) <- loop s m, (code2,b,s) <- loop s (f a) ]
      Emit i -> [([i],(),s)]
      Alts ms -> [ res | m <- ms, res <- loop s m ]
      FreshTemp -> do
        let State { temps } = s
        let (loc,temps') = nextTemp temps
        let s' = s { temps = temps' }
        [([],loc,s')]

data State = State { temps :: Temps }
data Temps = Temps [Loc]

nextTemp :: Temps -> (Loc,Temps)
nextTemp = \case
  Temps [] -> error "run out of temps"
  Temps (loc:locs) -> (loc,Temps locs)


----------------------------------------------------------------------
-- instruction

data Instruction
  = Tax | Tay | Txa | Tya
  | Clc
  | Adc_Z Byte | Adc_L Byte
  | Lda_Z Byte | Lda_L Byte
  | Inxy XY | Ldxy_Z XY Byte | Ldxy_L XY Byte
  | Inc_Z Byte
  | Sta_Z Byte
  | Stxy_Z XY Byte

-- capture commonality betweem two index regs
data XY = X | Y deriving (Eq,Ord)

instance Show Instruction where
  show = \case
    Tax -> "tax"
    Tay -> "tay"
    Txa -> "txa"
    Tya -> "tya"
    Clc -> "clc"
    Adc_L v -> printf "adc #%d" v
    Adc_Z z -> printf "adc %d" z
    Lda_L v -> printf "lda #%d" v
    Lda_Z z -> printf "lda %d" z
    Inxy xy -> printf "in%s" (show xy)
    Ldxy_Z xy z -> printf "ld%s %d" (show xy) z
    Ldxy_L{} -> undefined
    Inc_Z z -> printf "inc %d" z
    Sta_Z z -> printf "sta %d" z
    Stxy_Z xy z -> printf "st%s %d" (show xy) z

instance Show XY where
  show = \case X -> "x"; Y -> "y"

----------------------------------------------------------------------
-- TODO: evaluate language expressions
-- TODO: emulate code sequences -- check they match!
-- then can set up a bunch of tests!
-- and then I can attempt to optimize the IS with some hope ofmy mistakes being found!!
