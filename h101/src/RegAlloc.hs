
module RegAlloc where -- (main) where -- explore Register allocation ideas

import Control.Monad (ap,liftM)
import Data.Map (Map)
import Data.Set (Set,singleton,union,(\\))
import Text.Printf(printf)
import qualified Data.Map as Map
import qualified Data.Set as Set (empty)

main :: IO ()
main = do
  let _exp :: Exp = Sub (Sub (Var "a") (Var "b")) (Sub (Var "a") (Var "d"))
  let exp :: Exp = Let "e" (Sub (Var "a") (Var "b")) (Sub (Sub (Var "c") (Var "e")) (Sub (Var "e") (Var "d")))
  printf "example: %s\n" (show exp)
  let w = Map.fromList [("a",InMem "a"), ("b",InMem "b"), ("c",InMem "c"), ("d",InMem "d")]
  see "alloc1" (alloc1 w exp)
  see "alloc2" (alloc2 w exp)
  see "alloc3" (alloc3 w exp)

see :: String -> Asm () -> IO ()
see tag asm = do
  printf "%s...\n" tag
  let regs = [Ax,Bx,Cx,Dx]
  ops <- runAsm regs asm
  putStrLn (pp ops)

pp :: [Op] -> String
pp ops = unlines (map show ops)


type Where = Map Var Loc

-- combined
alloc3 :: Where -> Exp -> Asm ()
alloc3 w0 exp0 = allocTo w0 Si exp0
  where
    look x w = maybe err id $ Map.lookup x w where err = error "unknown var"

    allocTo :: Where -> Reg -> Exp -> Asm ()
    allocTo w target = \case
      Var x -> mov target (look x w)
      Sub e1 e2 -> do
        allocTo w target e1
        alloc w e2 $ \loc -> do
        Emit (Sub2 target loc)
      Let x rhs body -> do
        alloc w rhs $ \loc -> do
        let w' = Map.insert x loc w
        allocTo w' target body

    alloc :: Where -> Exp -> (Loc -> Asm ()) -> Asm ()
    alloc w e k = case e of
      Var x -> k (look x w) -- TODO: sensible opt, use where is currently is
      exp -> do
        pick exp $ \reg -> do
        allocTo w reg exp
        k (InReg reg)


-- destination driven
alloc2 :: Where -> Exp -> Asm ()
alloc2 w0 exp0 = allocTo w0 Si exp0
  where
    look x w = maybe err id $ Map.lookup x w where err = error "unknown var"
    allocTo :: Where -> Reg -> Exp -> Asm ()
    allocTo w target = \case
      Var x -> mov target (look x w)
      Sub e1 e2 -> do
        allocTo w target e1
        pick e1 $ \another -> do
        allocTo w another e2
        Emit (Sub2 target (InReg another))
      Let x rhs body -> do
        pick rhs $ \another -> do
        allocTo w another rhs
        let w' = Map.insert x (InReg another) w
        allocTo w' target body


-- simplistic botton up
alloc1 :: Where -> Exp -> Asm ()
alloc1 w0 exp0 = do
    loc <- alloc w0 exp0
    mov Si loc
  where
    look x w = maybe err id $ Map.lookup x w where err = error "unknown var"
    alloc :: Where -> Exp -> Asm Loc
    alloc w = \case
      Var x -> pure $ look x w
      Sub e1 e2 -> do
        loc1 <- alloc w e1
        pick e1 $ \r1 -> do
        mov r1 loc1
        loc2 <- alloc w e2
        pick e2 $ \r2 -> do
        mov r2 loc2
        Emit (Sub2 r1 (InReg r2))
        pure (InReg r1)
      Let x rhs body -> do
        loc1 <- alloc w rhs
        pick rhs $ \r1 -> do
        mov r1 loc1
        let w' = Map.insert x (InReg r1) w
        loc1 <- alloc w' body
        pure loc1


mov ::Reg -> Loc -> Asm ()
mov r loc = do
  let nop = case loc of InReg r2 -> r==r2; InMem{} -> False
  if False && nop then pure () else Emit (Mov r loc)


pick :: Exp -> (Reg -> Asm a) -> Asm a
pick exp k = PickFor exp k
--pick exp k = do r <- PickNew exp; k r -- TODO new idea


instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Emit :: Op -> Asm ()
  PickFor :: Exp -> (Reg -> Asm a) -> Asm a
  --PickNew :: Exp -> Asm Reg


runAsm :: [Reg] -> Asm ()  -> IO [Op]
runAsm regs0 m0 = loop regs0 m0 k0
  where
    k0 () = pure []
    loop :: [Reg] -> Asm a -> (a -> IO [Op]) -> IO [Op]
    loop regs = \case
      Ret x -> \k -> k x
      Bind m f -> \k -> loop regs m $ \a -> loop regs (f a) k
      Emit op -> \k -> do (op:) <$> k ()
      PickFor exp f -> \k ->
        case regs of
          [] -> error "no more regs"
          r:regs -> do
            printf "pick: %s for %s \n" (show r) (show exp)
            loop regs (f r) k


{-
-- dont use cont style, but emit style, allowing inspect what ops comes after
runAsm :: [Reg] -> Asm ()  -> IO [Op]
runAsm regs0 m0 = let ((),xs) = loop [] m0 in pure xs
  where
    loop :: [Op] -> Asm a -> (a,[Op])
    loop after = \case
      Ret x -> (x,after)
      Bind m f -> do
        let ~(a,after1) = loop after2 m
            ~(b,after2) = loop after (f a)
        (b,after1)
      Emit op -> ((),op:after)
      PickNew _exp -> do
        let used = freeInCode after
        let avail = [ r | r <- regs0, r `notElem` used ]
        let _r = case avail of [] -> error "no more regs"; r:_ -> r
        (_r,after) -- TODO: useing _r causses <<loop>>
-}

-- source
data Exp = Var Var | Sub Exp Exp | Let Var Exp Exp
type Var = String

instance Show Exp where
  show = \case
    Var x -> x
    Sub e1 e2 -> printf "(%s - %s)" (show e1) (show e2)
    Let x rhs body -> printf "(let %s = %s in %s)" x (show rhs) (show body)


-- target
data Op = Sub2 Reg Loc | Mov Reg Loc
data Loc = InReg Reg | InMem String
data Reg = Ax | Bx | Cx | Dx | Si deriving (Eq,Ord,Show)

instance Show Op where
  show = \case
    Mov r l -> "mov " ++ show r ++ ", " ++ show l
    Sub2 r l -> "sub " ++ show r ++ ", " ++ show l

instance Show Loc where show = \case InReg r -> show r; InMem m -> printf "M[%s]" m

freeInCode :: [Op] -> Set Reg
freeInCode = \case
  [] -> Set.empty
  Sub2 r loc : ops -> freeInLoc loc `union` (freeInCode ops \\ singleton r)
  Mov r loc : ops -> freeInLoc loc `union` (freeInCode ops \\ singleton r)

freeInLoc :: Loc -> Set Reg
freeInLoc = \case
  InReg r -> singleton r
  InMem{} -> Set.empty
