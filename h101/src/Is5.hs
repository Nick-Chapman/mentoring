
module Is5 (main) where

import Text.Printf (printf)
import Data.Word (Word8)
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad (ap,liftM)

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

type Byte = Word8

-- maps
look :: (Ord k, Show k) => String -> Map k v -> k -> v
look tag m k = maybe err id (Map.lookup k m) where err = error (show ("look",tag,k))

extend :: Ord k => Map k v -> k -> v -> Map k v
extend m k v = Map.insert k v m


main :: IO ()
main = do
  print "*Is4*"
  runTests

runTests :: IO ()
runTests = do

  let a = "a"
  let x = "x"
  let y = "y"
  let z = "z"
  let z2 = "z2"

  let env :: Env = Map.fromList [(a,RegA), (x,RegX), (y,RegY), (z,ZP 1), (z2,ZP 2)]
  let ee :: EvalEnv = Map.fromList [(a,13),(x,42),(y,101),(z,19),(z2,28)]

  let num n = Exp (Num n)
  let var x = Exp (Var x)
  let add e1 e2 = Exp (Op2 Add e1 e2)
  let asl e = Exp (Op1 Asl e)
  let let_ x e1 e2 = Exp (Let x e1 e2)

  let
    examples =
      [ num 77

      , var a
      , var x
      , var y
      , var z

      , add (var a) (num 1)
      , add (var x) (num 1)
      , add (var y) (num 1)
      , add (var z) (num 1)

      , add (num 1) (var a)
      , add (num 1) (var x)
      , add (num 1) (var y)
      , add (num 1) (var z)

      , add (var a) (num 2)
      , add (var x) (num 2)
      , add (var y) (num 2)
      , add (var z) (num 2)

      , add (num 3) (var a)
      , add (num 3) (var x)
      , add (num 3) (var y)
      , add (num 3) (var z)

      , add (var a) (var a)
      , add (var a) (var x)
      , add (var a) (var y)
      , add (var a) (var z)

      , add (var x) (var a)
      , add (var x) (var x)
      , add (var x) (var y)
      , add (var x) (var z)

      , add (var y) (var a)
      , add (var y) (var x)
      , add (var y) (var y)
      , add (var y) (var z)

      , add (var z) (var a)
      , add (var z) (var x)
      , add (var z) (var y)
      , add (var z) (var z)

      , add (var z) (var z2)
      , add (num 17) (num 19)
      , add (num 14) (num 1)
      , add (num 1) (num 14)

      , asl (num 14)
      , asl (var a)
      , asl (var x)
      , asl (var y)
      , asl (var z)

      , add (asl (var a)) (var a)
      , add (asl (var x)) (var x)
      , add (asl (var y)) (var y)
      , add (asl (var z)) (var z)

      , add (add (var a) (num 1)) (add (var a) (num 1))
      , add (add (num 17) (num 19)) (add (num 17) (num 19))

      , let_ "t1" (var a) (var "t1")
      , let_ "t1" (var x) (var "t1")
      , let_ "t1" (var y) (var "t1")
      , let_ "t1" (var z) (var "t1")

      , let_ "t1" (var a) (add (var "t1") (var "t1"))
      , let_ "t1" (var x) (add (var "t1") (var "t1"))
      , let_ "t1" (var y) (add (var "t1") (var "t1"))
      , let_ "t1" (var z) (add (var "t1") (var "t1"))

      , let_ "t1" (asl (add (num 1) (var z))) (add (var z2) (var "t1"))

      , add (num 1) (var z)

      ]

  printf "(eval)env = %s\n" (show ee)
  let ms0 :: MachineState = initMS env ee
  printf "ms0 = %s\n" (show ms0)

  let ss = semStateOfEnv env
  mapM_ (run1 ss ee ms0) examples


type Env = Map Var Loc

semStateOfEnv :: Env -> SemState
semStateOfEnv env = Map.fromList [ (loc,[Exp (Var x)]) | (x,loc) <- Map.toList env ]


run1 :: SemState -> EvalEnv -> MachineState -> Exp -> IO ()
run1 state ee ms0 example = do
  printf "\nexample = %s\n" (show example)
  let eres = eval ee example
  --printf "eres = %d\n" eres
  let temps1 = Temps [ZeroPage n | n <- [7..19]]
  let rs :: [Res] = runAsm temps1 state (compile0 example)
  let num = length rs
  if num == 0 then error "#results==0" else pure ()
  let
    prRes :: (Int, Res) -> IO ()
    prRes (i,(code,cost,arg)) = do
      let mres = emulate ms0 (code,arg)
      let same = (mres == eres)
      let ok :: String = if same then " {ok}" else printf " {FAIL: different: %d}" mres
      printf "#%d:{%s}: %s --> %s%s\n" i (show cost) (show code) (show arg) ok
      if not same then error "not ok" else pure ()

  mapM_ prRes (zip [1..] rs)


type Res = (Code,Cost,Arg)


compile0 :: Exp -> Asm Arg
compile0 exp = do
  perhaps spillA
  perhaps spillX
  perhaps spillY
  compile exp

compile :: Exp -> Asm Arg
compile exp@(Exp form) = do
  reusePreviousCompilation exp >>= \case
    Just arg -> pure arg
    Nothing ->
      case form of
        Num n -> pure (Imm (Immediate n))
        Var{} -> locations exp

        Op1 op1 exp1-> do
          arg <- compile exp1
          codegen exp (Op1 op1 arg)
          locations exp

        Op2 op2 exp1 exp2 -> do
          _ <- compile exp1
          arg2 <- compile exp2
          arg1 <- locations exp1
          codegen exp (Op2 op2 arg1 arg2)
          locations exp

        Let x rhs body -> do
          arg <- compile rhs
          link x arg
          --checkFreeVarsLocatable body -- shortcut the fail. nesc?
          compile body


{-
free :: Exp -> [Var]
free = undefined

checkFreeVarsLocatable :: Exp -> Asm ()
checkFreeVarsLocatable exp = do
  state <- GetSemState
  let xs = free exp
  if all id [ somewhere (locateV state x) | x <- xs ] then pure () else Nope

locateV :: SemState -> Var -> Located
locateV s x = locateE s (Exp (Var x))
-}


link :: Var -> Arg -> Asm ()
link x = \case
  Imm{} -> pure()
  Loc loc -> updateSemState (linkName x loc)

linkName :: Var -> Loc -> SemState -> SemState
linkName x loc s = do
  let e = Exp (Var x)
  let es = case Map.lookup loc s of Just es -> es; Nothing -> []
  extend s loc (e:es)

updateSemState :: (SemState -> SemState) -> Asm ()
updateSemState f = do
  ss <- GetSemState
  SetSemState (f ss)

locations :: Exp -> Asm Arg
locations exp = do
  state <- GetSemState
  let located = locateE state exp
  locationsL located

reusePreviousCompilation :: Exp -> Asm (Maybe Arg)
reusePreviousCompilation exp = do
  state <- GetSemState
  let located = locateE state exp
  if somewhere located then Just <$> (locationsL located) else pure Nothing

somewhere :: Located -> Bool
somewhere located = not (null (argsL located))

locationsL :: Located -> Asm Arg
locationsL located = alts [ pure arg | arg <- argsL located ]

data Located = LocatedList [Arg]

argsL :: Located -> [Arg]
argsL (LocatedList xs) = xs

locateE :: SemState -> Exp -> Located
locateE state exp =
  LocatedList
  ((case exp of Exp (Num b) -> [Imm (Immediate b)]; _ -> []) -- TODO: surprise we need this
   ++ [ Loc loc | (loc,exps) <- Map.toList state, exp `elem` exps ])


type SemState = Map Loc [Exp]

getS :: Loc -> SemState -> [Exp]
getS loc s = look "getS" s loc


alts :: [Asm a] -> Asm a
alts = foldl Alt Nope

----------------------------------------------------------------------
-- asm

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Alt :: Asm a -> Asm a -> Asm a
  Nope :: Asm a
  GetSemState :: Asm SemState
  SetSemState :: SemState -> Asm ()
  Fresh :: Asm ZeroPage
  Emit :: Instruction -> Semantics -> Asm ()


runAsm :: Temps -> SemState -> Asm a -> [(Code,Cost,a)]
runAsm temps0 ss0 asm0 = [ (code,q,a) | (code,q,_s,a) <- loop s0 asm0 ]
  where
    s0 = State { ss = ss0, temps = temps0 }

    loop :: State -> Asm a -> [(Code,Cost,State,a)]
    loop s@State{ss,temps} = \case
      Ret a -> [([],0,s,a)]
      Bind m f ->
        [(c1++c2,q1+q2,s,b) | (c1,q1,s,a) <- loop s m
                            , (c2,q2,s,b) <- loop s (f a) ]

      Alt m1 m2 -> loop s m1 ++ loop s m2
      Nope -> []

      GetSemState -> do
        [([],0,s,ss)]

      SetSemState ss -> do
        let s' = s { ss }
        [([],0,s',())]

      Fresh -> do
        case temps of
          Temps [] -> error "run out of temps"
          Temps (z:zs) -> do
            let s' = s { temps = Temps zs }
            [([],0,s',z)]

      Emit instruction semantics -> do
        let s' = s { ss = semantics ss }
        [ ([instruction], cost instruction, s', ()) ]


data State = State { ss :: SemState, temps :: Temps }

data Temps = Temps [ZeroPage]

----------------------------------------------------------------------
-- cost

newtype Cost = Cost Int deriving (Num)

cost :: Instruction -> Cost
cost _ = 1 -- TEMP -- better cycles or space

instance Show Cost where
  show (Cost n) = show n

----------------------------------------------------------------------
-- exp

data Exp = Exp (Form Exp)
  deriving (Eq)
data Form e = Var Var | Num Byte | Op2 Op2 e e | Op1 Op1 e | Let Var e e
  deriving (Eq)

data Op1 = Asl -- | Lsr
  deriving (Eq)
data Op2 = Add -- | Sub
  deriving (Eq)

type Var = String

----------------------------------------------------------------------
-- eval

type EvalEnv = Map Var Byte

eval :: EvalEnv -> Exp -> Byte
eval ee (Exp form) =
  case form of
    Num n -> n
    Var x -> look "eval" ee x
    Op1 Asl exp1 -> 2 * eval ee exp1
    Op2 Add exp1 exp2 -> eval ee exp1 + eval ee exp2
    Let x rhs body -> eval (extend ee x (eval ee rhs)) body

----------------------------------------------------------------------
-- show exp

instance Show Exp where
  show (Exp form) = show form

instance Show a => Show (Form a) where
  show = \case
    Num n -> show n
    Var x -> x
    Op1 Asl e -> printf "(%s << 1)" (show e)
    Op2 Add e1 e2 -> printf "(%s + %s)" (show e1) (show e2)
    Let x rhs body -> printf "(let %s = %s in %s)" x (show rhs) (show body)

----------------------------------------------------------------------
-- Locations

data Arg = Loc Loc | Imm Immediate

data Loc = RegA | RegX | RegY | ZP ZeroPage
  deriving (Eq,Ord)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    Loc loc -> show loc

instance Show Loc where
  show = \case
    RegA -> "A"
    RegX -> "X"
    RegY -> "Y"
    ZP z -> "ZP-" ++ show z


----------------------------------------------------------------------
-- emulate

data MachineState = MS { m :: Map Loc Byte } -- flags will go in here also
  deriving Show

initMS :: Env -> EvalEnv -> MachineState
initMS env ee = do
  let m = Map.fromList [ (loc,look "initMS" ee var) | (var,loc) <- Map.toList env ]
  MS m

emulate :: MachineState -> (Code,Arg) -> Byte
emulate ms0 (code,argFinal) = steps ms0 code
  where
    steps ms = \case
      [] -> get_arg ms argFinal
      i:is -> steps (step ms i) is

get_arg :: MachineState -> Arg -> Byte
get_arg MS{m} = \case
  Imm (Immediate b) -> b
  Loc loc -> look "get_arg" m loc

----------------------------------------------------------------------
-- Semantics

type Semantics = SemState -> SemState

noSemantics :: Semantics
noSemantics = id

transfer :: Loc -> Loc -> Semantics
transfer src dest = \s -> extend s dest (getS src s)

overwrite :: Exp -> Loc -> Semantics
overwrite e loc s = extend s loc [e]

----------------------------------------------------------------------
-- ZeroPage & Immediate

newtype ZeroPage = ZeroPage Byte
  deriving (Eq,Ord,Num)

newtype Immediate = Immediate Byte
  deriving (Eq,Ord,Num)

instance Show ZeroPage where
  show (ZeroPage z) = show z

instance Show Immediate where
  show (Immediate b) = "#" ++ show b

----------------------------------------------------------------------
-- instructions

type Code = [Instruction]

data Instruction = Tx ITransfer | Comp ICompute | Clc

data ITransfer
  = Tax | Txa | Tya
  | Ldai Immediate | Ldaz ZeroPage
  | Ldxi Immediate | Ldxz ZeroPage
  | Sta ZeroPage
  | Stx ZeroPage
  | Sty ZeroPage

data ICompute
  = Adcz ZeroPage
  | Adci Immediate
  | Inx
  | Incz ZeroPage
  | Asla

----------------------------------------------------------------------
-- show instruction

instance Show Instruction where
  show = \case
    Tx i -> show i
    Comp i -> show i
    Clc -> "clc"

instance Show ITransfer where
  show = \case
    Tax -> "tax"
    Txa -> "txa"
    Tya -> "tya"
    Ldai v -> printf "lda %s" (show v)
    Ldaz z -> printf "lda %s" (show z)
    Ldxi v -> printf "ldx %s" (show v)
    Ldxz z -> printf "ldx %s" (show z)
    Sta z -> printf "sta %s" (show z)
    Stx z -> printf "stx %s" (show z)
    Sty z -> printf "sty %s" (show z)

instance Show ICompute where
  show = \case
    Adci v -> printf "adc %s" (show v)
    Adcz z -> printf "adc %s" (show z)
    Inx -> "inx"
    Incz z -> printf "inc %s" (show z)
    Asla -> "asl a"

----------------------------------------------------------------------
-- instruction semantics

transferSemantics :: ITransfer -> Semantics
transferSemantics = \case
  Tax -> transfer RegA RegX
  Txa -> transfer RegX RegA
  Tya -> transfer RegY RegA
  Ldai (Immediate b) -> overwrite (Exp (Num b)) RegA
  Ldxi (Immediate b) -> overwrite (Exp (Num b)) RegX
  Ldaz z -> transfer (ZP z) RegA
  Ldxz z -> transfer (ZP z) RegX
  Sta z -> transfer RegA (ZP z)
  Stx z -> transfer RegX (ZP z)
  Sty z -> transfer RegY (ZP z)

computeSemantics :: Exp -> ICompute -> Semantics
computeSemantics e = \case
  Adci{} -> overwrite e RegA
  Adcz{} -> overwrite e RegA
  Inx -> overwrite e RegX
  Incz z -> overwrite e (ZP z)
  Asla -> overwrite e RegA

----------------------------------------------------------------------
-- step instruction (for emulate)

step :: MachineState -> Instruction -> MachineState
step ms@MS{m} = do
  --let get0 arg = get_arg ms arg
  let get loc = get_arg ms (Loc loc)
  let a = RegA
  let x = RegX
  let y = RegY
  let up k v = ms { m = extend m k v }
  \case
    Clc -> ms
    Tx i ->
      case i of
        Tax -> up x (get a)
        Txa -> up a (get x)
        Tya -> up a (get y)
        Ldai (Immediate b) -> up a b
        Ldxi (Immediate b) -> up x b
        Ldaz z -> up a (get (ZP z))
        Ldxz z -> up x (get (ZP z))
        Sta z -> up (ZP z) (get a)
        Stx z -> up (ZP z) (get x)
        Sty z -> up (ZP z) (get y)
    Comp i ->
      case i of
        Adci (Immediate b) -> up a (get a + b)
        Adcz z -> up a (get a + get (ZP z))
        Inx -> up x (get x + 1)
        Incz z -> up (ZP z) (get (ZP z) + 1)
        Asla -> up a (2 * get a)

----------------------------------------------------------------------
-- instruction selection

type Gen = Exp -> Form Arg -> Asm ()

select :: [Gen] -> Gen
select gs = \e f -> alts [ g e f | g <- gs ]

codegen :: Gen
codegen = select [doubling,addition,incrementX,incrementM]

doubling :: Gen
doubling e = \case
  Op1 Asl arg -> do loadA arg; comp e Asla
  _ -> Nope

addition :: Gen
addition e = \case
  Op2 Add (Loc RegA) arg -> do addIntoA e arg
  Op2 Add arg (Loc RegA) -> do addIntoA e arg
  Op2 Add arg1 arg2 -> do loadA arg1; addIntoA e arg2 --; perhaps spillA -- TODO
  _ -> Nope

addIntoA :: Exp -> Arg -> Asm ()
addIntoA e = \case
  Imm imm -> do clc; comp e (Adci imm)
  Loc RegA -> comp e (Asla)
  Loc RegX -> Nope
  Loc RegY -> Nope
  Loc (ZP z) -> do clc; comp e  (Adcz z)

incrementX :: Gen -- TODO Y like X
incrementX e = \case
  Op2 Add arg (Imm 1) -> do loadX arg; comp e Inx
  Op2 Add (Imm 1) arg -> do loadX arg; comp e Inx
  _ -> Nope

incrementM :: Gen
incrementM e = \case
  Op2 Add arg (Imm 1) -> do z <- inZP arg; comp e (Incz z)
  Op2 Add (Imm 1) arg -> do z <- inZP arg; comp e (Incz z)
  _ -> Nope

inZP :: Arg -> Asm ZeroPage
inZP = \case
  Loc (ZP z) -> pure z
  _ -> Nope

-- TODO: compile time constant folding

----------------------------------------------------------------------
-- codegen helpers

loadA :: Arg -> Asm ()
loadA = \case
  Imm imm -> trans (Ldai imm)
  Loc RegA -> pure ()
  Loc RegX -> trans Txa
  Loc RegY -> trans Tya
  Loc (ZP z) -> trans (Ldaz z)

loadX :: Arg -> Asm ()
loadX = \case
  Imm imm -> trans (Ldxi imm)
  Loc RegA -> trans Tax
  Loc RegX -> pure ()
  Loc RegY -> Nope -- no Y->X
  Loc (ZP z) -> trans (Ldxz z)

clc :: Asm ()
clc = Emit Clc noSemantics


trans :: ITransfer -> Asm ()
trans i = Emit (Tx i) (transferSemantics i)

comp :: Exp -> ICompute -> Asm ()
comp e i = Emit (Comp i) (computeSemantics e i)

----------------------------------------------------------------------
-- spilling...

perhaps :: Asm () -> Asm ()
perhaps a = alts [a, pure ()] -- shorter come last; nicer for early dev/debug

-- TODO: only spill if these location map to some expression

spillA :: Asm ()
spillA = do
  z <- Fresh
  trans (Sta z)

spillX :: Asm ()
spillX = do
  z <- Fresh
  trans (Stx z)

spillY :: Asm ()
spillY = do
  z <- Fresh
  trans (Sty z)
