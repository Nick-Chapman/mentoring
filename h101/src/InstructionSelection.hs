module InstructionSelection (main) where

import Data.Map (Map)
import Data.Word (Word8)
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Is2 (main)

type Byte = Word8

main :: IO ()
main = Is2.main

_main :: IO ()
_main = do
  print "*instruction-selection*"
  printf "env: %s\n"  (show env0)
  mapM_ (run env0) examples

env0 :: Env
env0 = Map.fromList [("x",ZP (ZeroPage 1)), ("y",ZP (ZeroPage 2))]

examples :: [Exp]
examples =
  [ add x y
  , add
    (add (num 11) (num 22))
    (add
     (add (num 33) (num 44))
     (add (num 55) (num 66)))
  ]
  where
    add = E_Op2 Add
    num = E_Lit
    x = var "x"
    y = var "y"
    var = E_Var

newtype Tmps = Tmps [ZeroPage] -- next zero page to use for temps

run :: Env -> Exp -> IO ()
run env exp = do
  printf "example: %s\n"  (show exp)
  let tmps = Tmps [ ZeroPage n | n <- [70..79] ] -- meh?
  let xs = compile tmps env exp
  mapM_ prRes xs
    where
      prRes :: (Code,Location,Tmps) -> IO ()
      prRes (code,loc,_) = do
        printf "--> %s\n" (show loc)
        mapM_ prI code
      prI :: Instruction -> IO ()
      prI i = printf "  %s\n" (show i)

compile :: Tmps -> Env -> Exp -> [(Code,Location,Tmps)]
compile tmps q exp =
  case exp of
    E_Lit v -> [([],Lit v,tmps)]
    E_Var x -> [([],loc,tmps)] where loc = maybe err id (Map.lookup x q)
                                     err = error (show ("compile/var",x))
    E_Op2 Add exp1 exp2 -> do
      (code1,loc1,tmps) <- compile tmps q exp1
      (code2,loc2,tmps) <- compile tmps q exp2
      (code2',loc1',tmps) <- [preserving tmps loc1 code2]
      (code3,loc) <- selectForAdd (loc1',loc2)
      [ (code1 ++ code2' ++ code3, loc, tmps) ]

preserving :: Tmps -> Location -> Code -> (Code,Location,Tmps)
preserving tmps x code =
  if x `notElem` trashes code
  then (code,x,tmps)
  else do
    let (code1,x',tmps') = spill tmps x
    (code1 ++ code, x', tmps')

spill :: Tmps -> Location -> (Code,Location,Tmps)
spill tmps = \case
  x@Lit{} -> ([],x,tmps)
  ZP{} -> undefined -- ???
  A -> do
    case tmps of
      Tmps [] -> error "splil: run out of temps"
      Tmps (z:zs) ->
        ([StaZ z],ZP z, Tmps zs)

selectForAdd :: (Location,Location) -> [(Code,Location)] -- choices. so far everything is deterministic
selectForAdd = \case
  (A,loc) -> [(addIntoAcc loc, A)]
  (loc, A) -> [(addIntoAcc loc, A)]
  (loc,ZP z) -> [(loadAcc loc ++ [Clc, AdcZ z], A)]
  (loc,Lit v) -> [(loadAcc loc ++ [Clc, AdcL v], A)]

addIntoAcc :: Location -> Code
addIntoAcc = \case
  A -> [Asl_A]
  ZP z -> [Clc, AdcZ z]
  Lit v -> [Clc, AdcL v]

loadAcc :: Location -> Code
loadAcc = \case
  A -> undefined []
  ZP z -> [LdaZ z]
  Lit v -> [LdaL v]

type Var = String

type Env = Map Var Location

data Exp
  = E_Var Var
  | E_Lit Byte
--  | E_Op1 Op1 Exp
  | E_Op2 Op2 Exp Exp
  -- E_Let

data Op2 = Add -- | Sub
--data Op1 = Lshift | Rshift

instance Show Exp where
  show = \case
    E_Var x -> x
    E_Lit v -> show v
    E_Op2 Add e1 e2 -> printf "(%s + %s)" (show e1) (show e2)

type Code = [Instruction]

data Instruction
  = LdaZ ZeroPage
  | LdaL Byte
  | StaZ ZeroPage
  | Clc
  | AdcZ ZeroPage
  | AdcL Byte
  | Asl_A
--  | Pha
--  | Pla
--  | Tax -- ...

data ZeroPage = ZeroPage Byte
  deriving Eq

data Location = A | Lit Byte | ZP ZeroPage -- X | Y |
  deriving Eq

trashes :: Code -> [Location]
trashes code = concat (map trashesI code)
  where
    trashesI :: Instruction -> [Location]
    trashesI = \case
      LdaL{} -> [A]
      LdaZ{} -> [A]
      AdcZ{} -> [A]
      AdcL{} -> [A]
      Asl_A -> [A]
      StaZ{} -> []
      Clc -> []
--      Pha -> []
--      Pla -> [A]

instance Show Instruction where
  show = \case
    LdaL v -> printf "lda #%s" (show v)
    LdaZ z -> printf "lda %s" (show z)
    StaZ z -> printf "sta %s" (show z)
    Clc -> "clc"
    AdcL v -> printf "adc #%s" (show v)
    AdcZ z -> printf "adc %s" (show z)
    Asl_A -> "asl a"
--    Pha -> "pha"
--    Pla -> "pla"

instance Show ZeroPage where
  show (ZeroPage n) = show n

instance Show Location where
  show = \case
    Lit v -> printf "lit-%s" (show v)
    ZP z -> printf "zp-%s" (show z)
    A -> printf "accumulator"
