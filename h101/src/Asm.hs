
module Asm where

import Control.Monad (ap,liftM)
import Data.Map (Map)
import qualified Data.Map as Map

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

data Instruction
data Flag
data State deriving (Eq,Ord)

trackEmit :: State -> Instruction -> State
trackEmit = undefined

data Price deriving (Eq,Ord)
price0 :: Price
account :: Price -> Instruction -> Price
(account,price0) = undefined

----------------------------------------------------------------------
data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Branch :: Flag -> Asm a -> Asm a -> Asm a
  Emit :: Instruction -> Asm ()
  Query :: Asm State
  Alt :: Asm a -> Asm a -> Asm a
  Nope :: Asm a

----------------------------------------------------------------------
-- construct simple linear code (alternation not supported)

data Code = Done | Do Instruction Code | Br Flag Code Code

runL :: State -> Asm () -> Code
runL s m = run m s (\_s () -> Done)
  where
    run :: forall a. Asm a -> State -> (State -> a -> Code) -> Code
    run = \case
      Ret a -> \s k -> k s a
      Bind m g -> \s k -> run m s $ \s a -> run (g a) s k
      Branch flag m1 m2 -> \s k -> Br flag (run m1 s k) (run m2 s k)
      Emit i -> \s k -> Do i $ k (trackEmit s i) ()
      Query -> \s k -> k s s
      Alt m1 m2 -> undefined m1 m2
      Nope -> undefined


----------------------------------------------------------------------
-- construct non-deterministic code-tree.

data NCode = NDone | NDo Instruction [NCode] | NBr Flag [NCode] [NCode]

expandN :: NCode -> [Code]
expandN = \case
  NDone -> [Done]
  NDo i ns -> [ Do i c | n <- ns, c <- expandN n ]
  NBr flag ns1 ns2 -> [ Br flag c1 c2 | n1 <- ns1, c1 <- expandN n1, n2 <- ns2, c2 <- expandN n2 ]

runN :: State -> Asm () -> [NCode]
runN s m = run m s (\_s () -> [NDone])
  where
    run :: forall a. Asm a -> State -> (State -> a -> [NCode]) -> [NCode]
    run = \case
      Ret a -> \s k -> k s a
      Bind m g -> \s k -> run m s $ \s a -> run (g a) s k
      Branch flag m1 m2 -> \s k -> [NBr flag (run m1 s k) (run m2 s k)]
      Emit i -> \s k -> [NDo i $ k (trackEmit s i) ()]
      Query -> \s k -> k s s
      Alt m1 m2 -> \s k -> run m1 s k ++ run m2 s k
      Nope -> \_s _k -> []

----------------------------------------------------------------------
-- construct non-deterministic code-tree, labelled with state and price info

data BCode = BDone | BDo Instruction State Price [BCode]

runB :: State -> Asm () -> [BCode]
runB s m = run m (s,price0) (\_sp () -> [BDone])
  where
    run :: forall a. Asm a -> (State,Price) -> ((State,Price) -> a -> [BCode]) -> [BCode]
    run = \case
      Ret a -> \sp k -> k sp a
      Bind m g -> \sp k -> run m sp $ \sp a -> run (g a) sp k
      Branch flag m1 m2 -> undefined flag m1 m2
      Emit i -> \(s,p) k -> do
        let s' = trackEmit s i
        let p' = account p i
        [BDo i s' p' $ k (s',p') ()]
      Query -> \(s,p) k -> k (s,p) s
      Alt m1 m2 -> \s k -> run m1 s k ++ run m2 s k
      Nope -> \_s _k -> []

----------------------------------------------------------------------
-- construct non-deterministic code-tree, labelled with state and price info
-- represented as a map from State

data MCode = MDone | MDo (Map State (Price,Instruction,MCode)) | MNope

expandM :: MCode -> [Code]
expandM = \case
  MDone -> [Done]
  MNope -> []
  MDo m -> [ Do i c | (_s,(_p,i,m)) <- Map.toList m, c <- expandM m ]

merge :: (MCode,MCode) -> MCode
merge = \case
  (MNope,m) -> m
  (m,MNope) -> m
  (MDone,_) -> MDone
  (_,MDone) -> MDone
  (MDo m1,MDo m2) ->
    MDo (Map.unionWith f m1 m2)
    where f v1@(p1,_,_) v2@(p2,_,_) = if p1 < p2 then v1 else v2

runM :: State -> Asm () -> MCode
runM s m = run m (s,price0) (\_sp () -> MDone)
  where
    run :: forall a. Asm a -> (State,Price) -> ((State,Price) -> a -> MCode) -> MCode
    run = \case
      Ret a -> \sp k -> k sp a
      Bind m g -> \sp k -> run m sp $ \sp a -> run (g a) sp k
      Emit i -> \(s,p) k -> do
        let s' = trackEmit s i
        let p' = account p i
        MDo (Map.singleton s' (p',i, k (s',p') ()))

      Query -> \(s,p) k -> k (s,p) s
      Alt m1 m2 -> \sp k -> merge (run m1 sp k, run m2 sp k)
      Nope -> \_sp _k -> MNope
      Branch flag m1 m2 -> undefined flag m1 m2



----------------------------------------------------------------------
template :: Asm a -> ()
template = undefined run
  where
    run = \case
      --Ret a -> undefined a
      --Bind m g -> undefined m g
      --Emit i -> undefined i
      --Query -> undefined
      --Alt m1 m2 -> undefined m1 m2
      --Nope -> undefined
      --Branch flag m1 m2 -> undefined flag m1 m2
      _ -> undefined

