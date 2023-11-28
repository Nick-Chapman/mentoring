
module MySet (main, Set, union, intersection, diff, remove) where

import Data.List.Ordered (isSorted)

main :: IO ()
main = do
  putStrLn "*MySet*"
  let big = fromList [1::Int ..100]
  see ("big",big)
  let reduced = foldr remove big [5..95]
  see ("reduced",reduced)
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

data Set a
  = Empty
  | Single a -- not required; just optimized rep
  | Node { left :: Set a
         , elem :: a
         , right :: Set a
         , sizeNode :: Int
         , heightNode :: Int
         }

nodeSH :: Ord a => Set a -> a -> Set a -> Set a
nodeSH left elem right =
  Node { left
       , elem
       , right
       , sizeNode = 1 + size left + size right
       , heightNode = 1 + max (height left) (height right)
       }

instance Show a => Show (Set a) where
  show = pretty

pretty :: Show a => Set a -> String -- show structure
pretty set = "{" ++ walk set ++ "}"
  where
    walk = \case
      Empty -> "."
      Single e -> "[" ++ show e ++ "]"
      Node{left=l,elem=e,right=r} ->
        "(" ++ walk l ++ show e ++ walk r ++ ")"

invariant :: Ord a => Set a -> Bool
invariant set = isSorted (toList set) -- TODO: and no dups.

toList :: Set a -> [a]
toList = walk []
  where
    walk :: [a] -> Set a -> [a]
    walk after = \case
      Empty -> after
      Single e -> e : after
      Node{left=l,elem=e,right=r} ->
        walk (e : walk after r) l

member :: Ord a => a -> Set a -> Bool
member x = \case
  Empty -> False
  Single e -> x==e
  Node{left=l,elem=e,right=r} ->
    x == e || member x (if x < e then l else r)

size :: Set a -> Int
size = \case
  Empty -> 0
  Single{} -> 1
  Node{sizeNode=x} -> x

height :: Set a -> Int
height = \case
  Empty -> 0
  Single{} -> 1
  Node{heightNode=x} -> x

empty :: Set a
empty = Empty

singleton :: a -> Set a
singleton = Single

{-singleton elem = Node
  { left = empty
  , elem
  , right = empty
  , sizeNode = 1
  , heightNode = 1
  }-}

twoSet :: Ord a => a -> a -> Set a
twoSet x y = -- x<y
  nodeSH empty x (singleton y)

insert :: Ord a => a -> Set a -> Set a
insert x = \case
  Empty -> singleton x
  n@(Single e) -> if x == e then n else if x < e then twoSet x e else twoSet e x
  n@Node{left=l,elem=e,right=r} -> if x == e then n else
    if x < e then mkNode (insert x l) e r else
    -- assert (x > e)
    mkNode l e (insert x r)

remove :: Ord a => a -> Set a -> Set a
remove x = \case
  Empty -> Empty
  n@(Single e) -> if x==e then empty else n
  Node {left=l,elem=e,right=r} ->
    if x < e then mkNode (remove x l) e r else
    if x > e then mkNode l e (remove x r) else
    -- assert (x==e)
    abut (l,r)

abut :: Ord a => (Set a,Set a) -> Set a
abut = \case
  (Empty,Empty) -> Empty
  (Empty,set) -> set
  (set,Empty) -> set
  (Single e1,Single e2) -> twoSet e1 e2
  (Single e1,n2@Node{}) -> mkNode empty e1 n2
  (n1@Node{},Single e2) -> mkNode n1 e2 empty

  (Node{left=l1,elem=e1,right=r1}, Node{left=l2,elem=e2,right=r2}) -> do
    -- assert (e1 < e2)
    mkNode l1 e1 (mkNode (abut (r1,l2)) e2 r2)

-- weaker; easier to maintain. still good for complexity
balanced :: Set a -> Bool
balanced = \case
  Empty -> True
  Single{} -> True
  Node {left=l,right=r} ->
    absdiff (height l) (height r) < 2
    && balanced l
    && balanced r
  where
    absdiff x y = if x < y then y-x else x-y

-- rebalance during node construction
mkNode :: Ord a => Set a -> a -> Set a -> Set a
mkNode l e r = case (l,r) of { (Empty,Empty) -> Single e ; _ -> do
  let u = height l - height r
  if u <=1 && u >= -1 then nodeSH l e r else
    if u < 0
    then
      case r of
        Empty -> undefined -- impossible: height(r) >= 2
        Single{} -> undefined -- impossible: height(r) >= 2
        Node {left=rl,elem=re,right=rr} -> nodeSH (nodeSH l e rl) re rr
    else
      case l of
        Empty -> undefined  -- impossible: height(l) >= 2
        Single{} -> undefined -- impossible: height(l) >= 2
        Node {left=ll,elem=le,right=lr} ->
          nodeSH ll le (nodeSH lr e r) }
