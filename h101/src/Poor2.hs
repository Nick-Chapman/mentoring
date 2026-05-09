module Poor2 (main) where

import Control.Monad (ap,liftM)

class Monad m => Writer m where
  write :: String -> m ()

class MonadTrans t where
  lift :: Monad m => m a -> t m a

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
runW = \case W _ s -> s


main :: IO ()
main = do
  putStrLn "**Poor Man's Concurrency"
  putStrLn $ take 70 $ runW $ runC example
  putStrLn "**DONE"

example :: Writer m => C m ()
example = do
  write "start"
  spawn (forever ".fish")
  forever ".cat"

spawn :: C m () -> C m ()
--spawn = Spawn -- primitive spawn
--spawn c = Fork c (pure ()) -- wrong: missing stop!
spawn c = Fork (do c; Stop) (pure ()) -- spawn in terms of Fork/Stop

forever :: Writer m => String -> m ()
forever s = do write s; forever s
--forever s = do write s; write s; write s


instance Functor (C m) where fmap = liftM
instance Applicative (C m) where(<*>) = ap; pure = Pure
instance Monad (C m) where (>>=) = Bind
instance MonadTrans C where lift = Lift
instance Writer m => Writer (C m) where write s = lift (write s)

data C m a where
  Pure :: a -> C m a
  Bind :: C m a -> (a -> C m b) -> C m b
  Lift :: m a -> C m a
  Spawn :: C m () -> C m ()
  Fork :: C m a -> C m a -> C m a
  Stop :: C m a

runC :: forall m. Monad m => C m () -> m ()
runC c = loop c (State []) kStop

kStop :: Monad m => () -> State m -> m ()
kStop = \() s -> resume s

loop :: Monad m => C m a -> State m -> (a -> State m -> m ()) -> m ()
loop c s k = case c of

  Pure a -> do
    k a s

  Bind c f -> do
    loop c s $ \a s -> loop (f a) s k

  Lift m -> do
    a <- m
    switch k a s

  Spawn c -> do
    {-switch-} k () (yield (\s -> loop c s kStop) s)

  Fork c1 c2 -> do -- duplicates k
    {-let this s = loop c1 s k
    let that s = loop c2 s k
    resume $ (yield this . yield that) s-}
    let s' = yield (\s -> loop c1 s k) s
    loop c2 s' k

  Stop -> do -- ignores k
    resume s

data State m = State [Cont m] -- use two lists to avoid tail-append in yield
type Cont m = State m -> m ()

switch :: Monad m => (a -> State m -> m ()) -> a -> State m -> m ()
--switch k a s = k a s -- dont switch
switch k a s = resume (yield (k a) s)

yield :: (State m -> m ()) -> State m -> State m
yield k (State ks) = State (ks++[k]) -- round robin

resume :: Monad m => State m -> m ()
resume (State ks) = case ks of
  [] -> pure () -- we're done
  k:ks -> k (State ks)



{-

_v1_runC :: forall m. Monad m => C m () -> m ()
_v1_runC c = round_robin [loop c kStop]
  where
    kStop () = AStop

    loop :: C m a -> (a -> Action m) -> Action m
    loop c k = case c of
      Pure a ->
        k a
      Bind m f ->
        loop m $ \a -> loop (f a) k
      Lift m ->
        AAtom (do a <- m; pure (k a))
      Spawn c ->
        AFork (loop c kStop) (k ())
      Fork c1 c2 ->
        AFork (loop c1 k) (loop c2 k) -- duplicates k
      Stop ->
        AStop -- ignores k

data Action m
  = AStop
  | AFork (Action m) (Action m)
  | AAtom (m (Action m))

round_robin :: Monad m => [Action m] -> m ()
round_robin = \case
  [] -> pure ()
  act:acts -> case act of
    AStop ->
      round_robin acts
    AFork act1 act2 ->
      round_robin (acts ++ [act1,act2])
    AAtom m -> do
      act <- m
      round_robin (acts ++ [act])

-}
