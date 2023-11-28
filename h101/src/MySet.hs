
module MySet (main, Set, union, intersection, diff, remove) where

import Data.List.Ordered (isSorted)

main :: IO ()
main = do
  putStrLn "*MySet*"
  see ("big",fromList [1::Int ..100])
  pure ()

  where
    see (tag,set) = do
      print (tag,"set:",set)
      print (tag,"size:",size set)
      print (tag,"height:",height set)
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
  show = pretty

pretty :: Show a => Set a -> String -- show structure
pretty set = "{" ++ walk set ++ "}"
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

height :: Set a -> Int -- TODO: should be constant time op
height = \case
  Empty -> 0
  Node l _ r -> 1 + max (height l) (height r)

empty :: Set a
empty = Empty

singleton :: a -> Set a
singleton x = Node empty x empty

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
    mkNode l1 e1 (mkNode (abut (r1,l2)) e2 r2) -- [2]


-- weaker; easier to maintain. still good for complexity
balanced :: Set a -> Bool
balanced = \case
  Empty -> True
  Node l _ r ->
    absdiff (height l) (height r) < 2
    && balanced l
    && balanced r
  where
    absdiff x y = if x < y then y-x else x-y

-- rebalance during node construction
mkNode :: Ord a => Set a -> a -> Set a -> Set a
mkNode l e r = do
  let u = height l - height r
  if u <=1 && u >= -1 then Node l e r else
    if u < 0
    then
      case r of
        Empty -> undefined -- impossible: height(r) >= 2
        Node rl re rr -> Node (Node l e rl) re rr
    else
      case l of
        Empty -> undefined  -- impossible: height(l) >= 2
        Node ll le lr ->
          Node ll le (Node lr e r)
