
module MotivateStateMonad (main) where

import Data.IORef (newIORef,readIORef,writeIORef)

import Control.Monad (ap,liftM)

expect :: (Eq a, Show a) => a -> a -> a
expect a b = if a == b then a else
  error ("expect failed: " ++ show a ++ " not same as: " ++ show b)

main :: IO ()
main = do
  let
    funky1 :: FunkyTree Char =
      FunkyBinNode
      (FunkyBinNodeWithData 'a'
       (FunkyLeaf 'b')
        (Funky3Way Nothing (FunkyEmpty,FunkyEmpty,FunkyEmpty)))
      (Funky3Way (Just 'c')
       ( FunkyLeaf 'd'
       , FunkyEmpty
       , Funky3Way (Just 'e') (FunkyEmpty,FunkyEmpty,FunkyEmpty)
       ))

  --print funky1

  let funky2 = mapFunky (\c -> [c,c]) funky1
  let
    funky2expect :: FunkyTree String =
      FunkyBinNode
      (FunkyBinNodeWithData "aa"
       (FunkyLeaf "bb")
        (Funky3Way Nothing (FunkyEmpty,FunkyEmpty,FunkyEmpty)))
      (Funky3Way (Just "cc")
       ( FunkyLeaf "dd"
       , FunkyEmpty
       , Funky3Way (Just "ee") (FunkyEmpty,FunkyEmpty,FunkyEmpty)
       ))

  print ("funky2", expect True (funky2expect == funky2))

  let funky3a = enumerateFunkyA funky2
  let
    funky3expect :: FunkyTree (Int,String) =
      FunkyBinNode
      (FunkyBinNodeWithData (2,"aa")
       (FunkyLeaf (1,"bb"))
        (Funky3Way Nothing (FunkyEmpty,FunkyEmpty,FunkyEmpty)))
      (Funky3Way (Just (5,"cc"))
       ( FunkyLeaf (3,"dd")
       , FunkyEmpty
       , Funky3Way (Just (4,"ee")) (FunkyEmpty,FunkyEmpty,FunkyEmpty)
       ))

  print ("funky3a", expect True (funky3expect == funky3a))

  let funky3b = enumerateFunkyB funky2
  print ("funky3b", expect True (funky3expect == funky3b))

  funky3io <- enumerateFunkyIO funky2
  print ("funky3io", expect True (funky3expect == funky3io))


  pure ()

{-
data SimpleTree a
  = SimpleBinNode (SimpleTree a) (SimpleTree a)
  | SimpleLeaf a
-}

data FunkyTree a
  = FunkyBinNode (FunkyTree a) (FunkyTree a)
  | FunkyBinNodeWithData a (FunkyTree a) (FunkyTree a)
  | FunkyLeaf a
  | FunkyEmpty
  | Funky3Way (Maybe a) (FunkyTree a, FunkyTree a, FunkyTree a)
  deriving (Eq, Show)

mapFunky :: (a -> b) -> FunkyTree a -> FunkyTree b
mapFunky f = \case

  FunkyBinNode left right ->
    FunkyBinNode (mapFunky f left) (mapFunky f right)

  FunkyBinNodeWithData a l r ->
    FunkyBinNodeWithData (f a) (mapFunky f l) (mapFunky f r)

  FunkyLeaf x ->
    FunkyLeaf (f x)

  FunkyEmpty ->
    FunkyEmpty

  Funky3Way ma (t1,t2,t3) -> do
    let ma' = case ma of Just a -> Just (f a); Nothing -> Nothing
    let t1' = mapFunky f t1
    let t2' = mapFunky f t2
    let t3' = mapFunky f t3
    Funky3Way ma' (t1',t2',t3')

{-
_mapFunky :: forall a b. (a -> b) -> FunkyTree a -> FunkyTree b
_mapFunky f = trans
  where
    trans :: FunkyTree a -> FunkyTree b
    trans = \case
      FunkyBinNode left right -> do
        FunkyBinNode (trans left) (trans right)

      FunkyBinNodeWithData a l r -> do
        let a' = f a
        let l' = trans l
        let r' = trans r
        FunkyBinNodeWithData a' l' r'

      FunkyLeaf x -> do
        let x' = f x
        FunkyLeaf x'

      FunkyEmpty -> do
        FunkyEmpty

      Funky3Way ma (t1,t2,t3) -> do
        let ma' = case ma of Just a -> Just (f a); Nothing -> Nothing
        let t1' = trans t1
        let t2' = trans t2
        let t3' = trans t3
        Funky3Way ma' (t1',t2',t3')
-}

-- left to right; children before parents
enumerateFunkyA :: forall a. FunkyTree a -> FunkyTree (Int,a)
enumerateFunkyA tree = snd (trans 1 tree)
  where
    trans :: Int -> FunkyTree a -> (Int, FunkyTree (Int,a))
    trans n = \case
      FunkyBinNode l r -> do
        let (n1,l') = trans n l
        let (n2,r') = trans n1 r
        (n2, FunkyBinNode l' r')

      FunkyBinNodeWithData a l r -> do
        let (n1,l') = trans n l
        let (n2,r') = trans n1 r
        (n2+1, FunkyBinNodeWithData (n2,a) l' r')

      FunkyLeaf x -> do
        (n+1, FunkyLeaf (n,x))

      FunkyEmpty -> do
        (n, FunkyEmpty)

      Funky3Way ma (t1,t2,t3) -> do
        let (n1,t1') = trans n t1
        let (n2,t2') = trans n1 t2
        let (n3,t3') = trans n2 t3
        let (n4,ma') = case ma of
              Just a -> (n3+1,Just (n3,a))
              Nothing -> (n3, Nothing)
        (n4, Funky3Way ma' (t1',t2',t3'))




enumerateFunkyB :: forall a. FunkyTree a -> FunkyTree (Int,a)
enumerateFunkyB tree = runC (trans tree)
  where
    trans :: FunkyTree a -> Counting (FunkyTree (Int,a))
    trans = \case
{-      FunkyBinNode l r ->
        trans l `bindC` \l ->
        trans r `bindC` \r ->
        pureC (FunkyBinNode l r)
-}
      FunkyBinNode l r -> do
        l' <- trans l
        r' <- trans r
        pure (FunkyBinNode l' r')

      FunkyBinNodeWithData a l r -> do
        l' <- trans l
        r' <- trans r
        n <- nextCount
        pure (FunkyBinNodeWithData (n,a) l' r')

      FunkyLeaf x -> do
        nextCount >>= \n ->
          pure $ FunkyLeaf (n,x)

      FunkyEmpty -> do
        pureC FunkyEmpty

      Funky3Way m (a,b,c) -> do
        a' <- trans a
        b' <- trans b
        c' <- trans c
        m' <- case m of
                Nothing -> pure Nothing
                Just a -> do
                  n <- nextCount
                  pure (Just (n,a))

        pure $ Funky3Way m' (a',b',c')



instance Functor Counting where fmap = liftM
instance Applicative Counting where pure = pureC; (<*>) = ap
instance Monad Counting where (>>=) = bindC

{-
instance Functor Counting where fmap = undefined mapC --liftM
instance Applicative Counting where pure = pureC; (<*>) = apC --ap
instance Monad Counting where (>>=) = bindC

apC :: Counting (a -> b) -> Counting a -> Counting b
apC = undefined

mapC :: (a -> b) -> Counting a -> Counting b
mapC f counting = counting `bindC` \a -> pureC (f a)
-}

{-
data Counting a = XX a

pureC :: a -> Counting a
pureC a = XX a

nextCount :: Counting Int
nextCount = XX 42 -- ???

runC :: Counting a -> a
runC (XX a) = a

bindC :: Counting a -> (a -> Counting b) -> Counting b
bindC (XX a) fb = fb a

-}


data Counting a = Counting (Int -> (Int,a))

pureC :: a -> Counting a
pureC a = Counting (\n -> (n,a))

nextCount :: Counting Int
nextCount = Counting (\n -> (n+1,n))

runC :: Counting a -> a
runC (Counting f) = let (_,res) = f 1 in res

bindC :: Counting a -> (a -> Counting b) -> Counting b
bindC (Counting f) bf =
  Counting (\n ->
              let (n',a) = f n in
              let (Counting g) = bf a in
              g n'
           )



enumerateFunkyIO :: forall a. FunkyTree a -> IO (FunkyTree (Int,a))
enumerateFunkyIO tree = do
  ref <- newIORef (1::Int)
  let
    nextCount :: IO Int
    nextCount = do
      n <- readIORef ref
      writeIORef ref (n+1)
      pure n
  let
    trans :: FunkyTree a -> IO (FunkyTree (Int,a))
    trans = \case
      FunkyBinNode l r -> do
        l' <- trans l
        r' <- trans r
        pure (FunkyBinNode l' r')

      FunkyBinNodeWithData a l r -> do
        l' <- trans l
        r' <- trans r
        n <- nextCount
        pure (FunkyBinNodeWithData (n,a) l' r')

      FunkyLeaf x -> do
        nextCount >>= \n ->
          pure $ FunkyLeaf (n,x)

      FunkyEmpty -> do
        pure FunkyEmpty

      Funky3Way m (a,b,c) -> do
        a' <- trans a
        b' <- trans b
        c' <- trans c
        m' <- case m of
                Nothing -> pure Nothing
                Just a -> do
                  n <- nextCount
                  pure (Just (n,a))

        pure $ Funky3Way m' (a',b',c')

  trans tree
