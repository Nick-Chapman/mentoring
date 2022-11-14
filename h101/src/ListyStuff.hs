module ListyStuff (main)  where

--import Prelude hiding(length,reverse)

main :: IO ()
main = do
  putStrLn "*h101*"
  let mylist = ["one","two","three","four"]
  print mylist
  print (reverse mylist)
  print (foo mylist)
  print (map reverse mylist)
  let lala = add 3
  print (mymap lala [1,2,3,4,5])
  contents :: String <- readFile "/usr/share/dict/words"
  let res = myfilter special (lines contents)
  print (length res)
  print "stop"

special :: String -> Bool
special xs =
  case xs of
    [] -> False
    'z':_ -> True
    _:_ -> False

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f xs =
  case xs of
    [] -> []
    x:xs ->
      case f x of
        True -> x : myfilter f xs
        False -> myfilter f xs

      --Silly version:  (if f x then (x:) else \x -> x)  (myfilter f xs)

add :: Int -> (Int -> Int)
add x y = x + y

foo :: [String] -> [Int]
foo xs = map length xs

mymap :: (a -> b) -> [a] -> [b]
mymap f xs =
  case xs of
    [] -> []
    first:rest -> f first : mymap f rest

{-
length :: String -> Int
length xs =
  case xs of
    [] -> 0
    _:xs -> 1 + length xs

foo :: [String] -> [Int]
foo xs =
  case xs of
    [] -> []
    first:rest -> length first : foo rest

bar :: [String] -> [String]
bar xs =
  case xs of
    [] -> []
    first:rest -> reverse first : bar rest

reverse :: [a] -> [a]
reverse xs =
  case xs of
    [] -> []
    x:xs -> snoc (reverse xs) x

snoc :: [a] -> a -> [a]
snoc xs x = append xs [x]

append :: [a] -> [a] -> [a]
append xs ys =
  case xs of
    [] -> ys
    x:xs -> x : append xs ys
-}
