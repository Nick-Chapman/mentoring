module Is2 (main) where

import Text.Printf (printf)
import Data.Word (Word8)
import Data.Map (Map)
import qualified Data.Map as Map

main :: IO ()
main = do
  print "*Is2*"
  run1

type Byte = Word8

look :: (Ord k, Show k) => Map k v -> k -> v
look m k = maybe err id (Map.lookup k m)
  where err = error (show ("look",k))

extend :: Ord k => Map k v -> k -> v -> Map k v
extend m k v = Map.insert k v m

-- language
type Var8 = String
data Oper8 = Add Atom8 Atom8 | Double Atom8 deriving Show
data Atom8 = Var Var8 | Num Byte deriving Show

{-
type Var1 = String
data Oper1 = EqualZero Atom8
data Exp
  = Ret Var8
  | Let Var8 Oper8 Var8
  | If Oper1 Oper8 Oper8
-}

data I = Inx | Ldx_Z Byte | Ldx_L Byte | Tax -- | Stx_Z Byte
  | Inc_Z Byte
  deriving Show
type Code = [I]

data Loc8 = A | X | Z Byte | L Byte deriving (Eq,Ord,Show)
data Loc1 = FZ {- | FN -} deriving (Eq,Ord)

-- inx
-- declare its applibility/introduction + its effect

-- compile-time env: for each var, where might it be at runtime
type EnvC = Map Var8 Loc8

-- runtime env; what value (i.e. a program variable) is held by each runtime location
type EnvR = (Map Loc8 Var8, Map Loc1 Test1)
data Test1 = IsZero Var8 -- | IsNeg Var8

-- generate result
type Res = (Code,EnvR,Loc8)

generate :: EnvC -> EnvR -> Var8 -> Oper8 -> [Res]
generate ec rt target oper =
  [ r | g <- [genInx,genInc], r <- g ec rt target oper ]

genInx :: EnvC -> EnvR -> Var8 -> Oper8 -> [Res]
genInx ec er target oper8 = do
  case oper8 of
    Double{} -> []
    Add pA qA -> do
      let pL = locate ec pA
      let qL = locate ec qA
      case (pL,qL) of
        (q, L 1) -> inc q
        (L 1, q) -> inc q
        _ -> [] -- not applicable
        where
          inc :: Loc8 -> [Res]
          inc q = do
            let code = loadX q ++ [Inx]
            let er' = extendER er
            [(code, er', X)]
            where
              extendER :: EnvR -> EnvR
              extendER (m8,m1) = (extend m8 X target, extend m1 FZ (IsZero target))

genInc :: EnvC -> EnvR -> Var8 -> Oper8 -> [Res]
genInc ec er target oper8 = do
  case oper8 of
    Double{} -> []
    Add pA qA -> do
      let pL = locate ec pA
      let qL = locate ec qA
      case (pL,qL) of
        (Z b, L 1) -> inc b
        (L 1, Z b) -> inc b
        _ -> [] -- not applicable
        where
          inc :: Byte -> [Res]
          inc b = do
            let code = [Inc_Z b]
            let er' = extendER er
            [(code, er', loc)]
            where
              loc = Z b
              extendER :: EnvR -> EnvR
              extendER (m8,m1) = (extend m8 loc target, m1)


loadX :: Loc8 -> Code
loadX = \case
  X -> undefined [] -- ever called?
  A -> undefined [Tax] -- ever called?
  Z b -> [Ldx_Z b]
  L b -> [Ldx_L b]

locate :: EnvC -> Atom8 -> Loc8
locate ec = \case
  Var x -> look ec x
  Num b -> L b


run1 :: IO ()
run1 = do
  let v1 = "v1"
  let v2 = "v2"
  let oper = Add (Var v1) (Num 1)
  printf "oper = %s\n" (show oper)
  let target = v2
  let ec = Map.fromList [(v1,Z 1)]
  printf "ec = %s\n" (show ec)
  let er = (Map.empty, Map.empty)
  let rs :: [Res] = generate ec er target oper
  printf "#results = %d\n" (length rs)
  mapM_ prRes rs
    where
      prRes :: Res -> IO ()
      prRes (code,_,loc) = do
        printf "%s --> %s\n" (show code) (show loc)
        --mapM_ prI code
      --prI :: I -> IO ()
      --prI i = printf "  %s\n" (show i)
