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
  spawn (loop ".fish")
  loop ".cat"

spawn :: C m () -> C m ()
--spawn = Spawn
--spawn c = Fork c (pure ())
spawn c = Fork (do c; Stop) (pure ())

loop :: Writer m => String -> m ()
loop s = do write s; loop s
--loop s = do write s; write s; write s


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
runC c = round_robin [loop c kStop]
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
