module MySet (main) where

import Data.List.Ordered (isSorted)

main :: IO ()
main = do
  putStrLn "*MySet*"

  let set1 = fromList [1::Int,2,5,4,3,2,6]
  see ("set1",set1)
  let set2 = remove 3 set1
  see ("set2",set2)

  let (_,set3) = case removeMin set1 of Just x -> x; Nothing -> undefined
  see ("set3",set3)
  let (set4,_) = case removeMax set1 of Just x -> x; Nothing -> undefined
  see ("set4",set4)

  let a = fromList [3::Int,6,9]
  let b = fromList [2,6]
  see ("A",a)
  see ("B",b)
  see ("U",union a b)
  see ("I",intersection a b)
  see ("D",diff a b)

  see ("big",fromList [1::Int ..100])

  -- More methodical testing

  pure ()

  where
    see (tag,set) = do
      print (tag,"set:",set)
      print (tag,"size:",size set)
      print (tag,"invariant:",check True (invariant set))
      print (tag,"balanced?", check True (balanced set))
      pure ()

check :: (Eq a, Show a) => a -> a -> a
check a b = if a == b then a else error ("check failed: " ++ show a ++ " not same as: " ++ show b)

union :: Ord a => Set a -> Set a -> Set a
union x y = if size x < size y then f x y else f y x
  where
    f smaller bigger =
      foldr insert bigger (toList smaller)

intersection :: Ord a => Set a -> Set a -> Set a
intersection x y = if size x < size y then f x y else f y x
  where
    f smaller bigger =
      fromList [ x | x <- toList smaller, x `member` bigger ]

diff :: Ord a => Set a -> Set a -> Set a
diff x y =
  fromList [ e | e <- toList x, not (e `member` y) ]

fromList :: Ord a => [a] -> Set a
fromList = foldr insert empty

data Set a = Empty | Node (Set a) a (Set a)
-- TODO: optimize with a special case for singletons...
-- Single a === Node Empty a Empty

instance Show a => Show (Set a) where
  show = _pretty
  --show = show . toList

_pretty :: Show a => Set a -> String -- show structure
_pretty set = "{" ++ walk set ++ "}"
  where
    walk = \case
      Empty -> "."
      Node l e r -> "(" ++ walk l ++ show e ++ walk r ++ ")"


invariant :: Ord a => Set a -> Bool
invariant set = isSorted (toList set) -- TODO: and no dups.

toList :: Set a -> [a]
toList = walk []
  where
    walk :: [a] -> Set a -> [a]
    walk after = \case
      Empty -> after
      Node l e r -> walk (e : walk after r) l

member :: Ord a => a -> Set a -> Bool
member x = \case
  Empty -> False
  Node l e r -> x == e || member x (if x < e then l else r)

size :: Set a -> Int -- TODO: should be constant time op
size = \case
  Empty -> 0
  Node l _ r -> size l + 1 + size r

empty :: Set a
empty = Empty

singleton :: a -> Set a
singleton x = Node empty x empty

balanced :: Set a -> Bool
balanced = \case
  Empty -> True
  Node l _ r ->
    absdiff (size l) (size r) < 2
    && balanced l
    && balanced r
  where
    absdiff x y = if x < y then y-x else x-y

insert :: Ord a => a -> Set a -> Set a
insert x = \case
  Empty -> singleton x
  n@(Node l e r) -> if x == e then n else
    if x < e then mkNode (insert x l) e r else
    -- assert (x > e)
    mkNode l e (insert x r)

remove :: Ord a => a -> Set a -> Set a
remove x = \case
  Empty -> Empty
  Node l e r ->
    if x < e then mkNode (remove x l) e r else
    if x > e then mkNode l e (remove x r) else
    -- assert (x==e)
    abut (l,r)

abut :: Ord a => (Set a,Set a) -> Set a
abut = \case
  (Empty,Empty) -> Empty
  (Empty,set@Node{}) -> set
  (set@Node{},Empty) -> set
  (Node l1 e1 r1, Node l2 e2 r2) -> do
    -- assert (e1 < e2)
    -- pick one of the two alternatives....
    -- mkNode (mkNode l1 e1 (abut (r1,l2))) e2 r2 -- [1]
    mkNode l1 e1 (mkNode (abut (r1,l2)) e2 r2) -- [2]


-- rebalance during node construction

mkNode :: Ord a => Set a -> a -> Set a -> Set a
mkNode l e r = do
  let u = size l - size r
  if u <=1 && u >= -1 then Node l e r else
    if u < 0 then
      case removeMin r of
        Nothing -> undefined -- (size r >= 2)
        Just (m,r) -> mkNode (insert e l) m r
    else
      case removeMax l of
        Nothing -> undefined -- (size l >= 2)
        Just (l,m) -> mkNode l m (insert e r)

removeMin :: Ord a => Set a -> Maybe (a,Set a)
removeMin = \case
  Empty -> Nothing
  Node l e r ->
    case removeMin l of
      Just (m,l) -> Just (m, mkNode l e r)
      Nothing -> Just (e,r)

removeMax :: Ord a => Set a -> Maybe (Set a,a)
removeMax = \case
  Empty -> Nothing
  Node l e r ->
    case removeMax r of
      Just (r,m) -> Just (mkNode l e r, m)
      Nothing -> Just (l,e)
