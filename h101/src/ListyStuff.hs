module ListyStuff (main) where

--import Prelude (IO,String,Bool(..),Int,print,readFile,lines,(+),(>),(<),(-),(*),(<=),(==),(&&))

--import Prelude hiding (map,filter,length,reverse,zip,take,drop,concat)
import Prelude hiding (zip,take,drop,concat)

main :: IO ()
main = do
  contents :: String <- readFile "/usr/share/dict/words"
  let res = filter (\w -> length w > 20) (lines contents)
  print (length res)

  print (upto 10 20)
  -- [10,11,12,13,14,15,16,17,18,19,20]

  print (upto 20 10)
  -- []

  print (zip (upto 1 20) ["fe","fi","fo","fum"])
  -- [(1,"fe"),(2,"fi"),(3,"fo"),(4,"fum")]

  print (take 5 (upto 100 10000000))
  -- [100,101,102,103,104]

  print (take 5 (drop 10 (upto 1 10000000)))
  -- [11,12,13,14,15]

  print (concat [ upto 1 3, upto 200 100, upto 7 8, upto 15 18 ])
  -- [1,2,3,7,8,15,16,17,18]

  print (cart (upto 1 3) ["fe","fi","fo","fum"])
  -- [(1,"fe"),(1,"fi"),(1,"fo"),(1,"fum"),(2,"fe"),(2,"fi"),(2,"fo"),(2,"fum"),(3,"fe"),(3,"fi"),(3,"fo"),(3,"fum")]

  print (cartHelper ["fe","fi","fo","fum"] 'X')
  -- [('X',"fe"),('X',"fi"),('X',"fo"),('X',"fum")]

  print (pythag 15)
  -- [(3,4,5),(5,12,13),(6,8,10),(9,12,15)]


pythag :: Int -> [(Int,Int,Int)]
pythag n = do
  let xs = upto 1 n
  let trips = map mkTrip (cart (cart xs xs) xs)
  filter isPythag trips

isPythag :: (Int,Int,Int) -> Bool
isPythag (x,y,z) = x*x + y*y == z*z && x<=y && y<=z

mkTrip :: ((a,b),c) -> (a,b,c)
mkTrip ((a,b),c) = (a,b,c)


upto :: Int -> Int -> [Int]
upto a b =
  if a > b then [] else a : upto (a+1) b

zip :: [a] -> [b] -> [(a,b)]
zip xs ys =
  case xs of
    [] -> []
    x:xs -> case ys of
      [] -> []
      y:ys -> (x,y) : zip xs ys

take :: Int -> [a] -> [a]
take n xs =
  if n < 1 then [] else
    case xs of
      [] -> []
      x:xs -> x : take (n-1) xs

drop :: Int -> [a] -> [a]
drop n xs =
  if n < 1 then xs else
    case xs of
      [] -> []
      _:xs -> drop (n-1) xs

concat :: [[a]] -> [a]
concat xss =
  case xss of
    [] -> []
    xs:xss -> xs ++ concat xss


cart :: [a] -> [b] -> [(a,b)]
cart xs ys = concat (map (cartHelper ys) xs)

cartHelper :: [b] -> a -> [(a,b)]
cartHelper ys x = map (\y -> (x,y)) ys


----------------------------------------------------------------------
{-
map :: (a -> b) -> [a] -> [b]
map f xs =
  case xs of
    [] -> []
    x:xs -> f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter f xs =
  case xs of
    [] -> []
    x:xs ->
      case f x of
        True -> x : filter f xs
        False -> filter f xs

length :: [a] -> Int
length xs =
  case xs of
    [] -> 0
    _:xs -> 1 + length xs

reverse :: [a] -> [a]
reverse xs =
  case xs of
    [] -> []
    x:xs -> append (reverse xs) [x]

append :: [a] -> [a] -> [a]
append xs ys =
  case xs of
    [] -> ys
    x:xs -> x : append xs ys
-}
