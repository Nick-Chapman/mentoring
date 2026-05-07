module Poor (main) where

import Control.Monad (ap,liftM)

class Monad m => Writer m where
  write :: String -> m ()

class MonadTrans t where
  lift :: Monad m => m a -> t m a


main :: IO ()
main = do
  putStrLn "**Poor Man's Concurrency"
  let s :: String = runW (runC example)
  putStrLn s
  putStrLn "**DONE"
  pure ()

example :: Writer m => C m ()
example = do
  write "start"
  fork (loop 7 ".fish")
  loop (5) ".cat"
  -- Change to (-5) -- we see no output. either W or C is not lazy enough

loop :: Writer m => Int -> String -> C m () -- negative gives infinite loop
loop i s = if i == 0 then pure () else do write s; loop (i-1) s


data W a = W a String

instance Functor W where fmap = liftM
instance Applicative W where
  (<*>) = ap
  pure a = W a ""
instance Monad W where
  (>>=) (W a s1) f = let W b s2 = f a in W b (s1++s2)

instance Writer W where
  write s = W () s

runW :: W () -> String
runW = \case W () s -> s


data C m a = C { appC :: ((a -> Action m) -> Action m) }

instance Functor (C m) where fmap = liftM
instance Applicative (C m) where
  (<*>) = ap
  pure a = C $ \k -> k a
instance Monad (C m) where
  (>>=) m g = C $ \k -> appC m $ \a -> appC (g a) k

instance Writer m => Writer (C m) where
  write s = lift (write s)

  -- character interleaving
  --write = \case [] -> pure (); x:xs -> do lift (write [x]); write xs

  -- provoke error in version of atom which uses Atom2
  -- write s = do b <- lift (do write s; pure True); if b then pure () else undefined

instance MonadTrans C where
  lift = atom


runC :: Monad m => C m () -> m ()
runC c = round_robin [actionC c]

actionC :: Monad m => C m () -> Action m
actionC c = appC c $ \() -> Stop

atom :: Monad m => m a -> C m a
atom m = C $ \k -> Atom1 (do a <- m; pure (k a)) -- This is very tricksy
--atom m = C $ \k -> Atom2 (do _a <- m; pure ()) (k undefined)

-- _stop :: Monad m => C m a
-- _stop = C $ \_k -> Stop

_par :: Monad m => C m a -> C m a -> C m a
_par = undefined

fork :: Monad m => C m () -> C m ()
fork c = C $ \k -> Fork (actionC c) (k ())


data Action m
  = Stop
  | Fork (Action m) (Action m)
  | Atom1 (m (Action m)) -- This type is confusing for me.
--  | Atom2 (m ()) (Action m) -- can't wrte lift

round_robin :: Monad m => [Action m] -> m ()
round_robin = \case
  [] -> pure ()
  act:acts -> case act of
    Stop ->
      round_robin acts
    Fork act1 act2 ->
      round_robin (acts ++ [act1,act2])
    Atom1 m -> do
      act <- m
      round_robin (acts ++ [act])

    --Atom2 m act -> do
    --  m
    --  round_robin (acts ++ [act])
