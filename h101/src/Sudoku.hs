module Sudoku (main) where

import Data.Char (ord)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Ord (comparing)
import Data.Set (Set,union,(\\))
import Data.Set as Set (toList,fromList)
import qualified Data.Map as Map

import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let filename = parseCommandLine args
  s <- readFile filename
  let g = parse s
  print g
{-
  loop 0 g >>= \case
    Nothing -> print "no solution"
    Just g' ->
      print g'
-}
  sols :: [Grid] <- loopN 0 g
  print (length sols)


parseCommandLine :: [String] -> String
parseCommandLine = \case
  [x] -> x
  args -> error (show ("parseCommandLine",args))

parse :: String -> Grid
parse s = do
  Grid $ Map.fromList $ [ (Pos {x,y},mkDigit c)
                        | (y,cs) <- zip [1..] $ lines s
                        , (x,c) <- zip [1..] cs
                        , c /= '.' ]

trace :: Show a => a -> IO ()
--trace x = print x
trace _ = pure ()

{-
loop :: Int -> Grid -> IO (Maybe Grid)
loop i g = do
  case infer g of
    Done -> do
      pure (Just g)
    Fail -> do
      trace ("step", i,"Fail")
      pure Nothing
    Infer p d -> do
      trace ("step", i,"infer",p,d)
      let g' = extendG g (p,d)
      loop (i+1) g'
    Choice p ds -> do
      trace ("step", i,"Choice",p,"alts=",ds)
      let
        inner :: [(Int,Digit)] -> IO (Maybe Grid)
        inner = \case
          [] -> pure Nothing
          (_j,d1):ds -> do
            trace ("step", i,"Choice",p,"try#",_j, "digit=",d1)
            loop (i+1) (extendG g (p,d1)) >>= \case
              Nothing -> inner ds
              Just g' -> pure (Just g')
        in
        inner (zip [1..] ds)
-}

loopN :: Int -> Grid -> IO [Grid]
loopN i g = do
  case infer g of
    Done -> do
      print g
      pure [g]
    Fail -> do
      trace ("step", i,"Fail")
      pure []
    Infer p d -> do
      trace ("step", i,"infer",p,d)
      let g' = extendG g (p,d)
      loopN (i+1) g'
    Choice p ds -> do
      trace ("step", i,"Choice",p,"alts=",ds)
      let
        inner :: [(Int,Digit)] -> IO [Grid]
        inner = \case
          [] -> pure []
          (_j,d1):ds -> do
            trace ("step", i,"Choice",p,"try#",_j, "digit=",d1)
            sols1 <- loopN (i+1) (extendG g (p,d1))
            sols2 <- inner ds
            pure (sols1 ++ sols2)
        in
        inner (zip [1..] ds)




data Res = Done | Fail | Infer Pos Digit | Choice Pos [Digit]

infer :: Grid -> Res
infer g = do
  let m = [ (p, Set.toList (allowed g p)) | p <- allPos , not (filled g p)]
  case [ p | (p,[]) <- m ] of
    _:_ -> Fail
    [] -> do
      let xs = [ (p,d) | (p,[d]) <- m ]
      case xs of
        (p,d):_ -> Infer p d
        [] -> do
          case m of
            [] -> Done
            _:_ -> do
              let xs = [(p,ds,length ds) | (p,ds) <- m ]
              let third (_,_,n) = n
              let xs' = sortBy (comparing third) xs
              let (p,ds,_) = head xs'
              Choice p ds


allowed :: Grid -> Pos -> Set Digit
allowed g p = allDigits \\ disallowed g p

disallowed :: Grid -> Pos -> Set Digit
disallowed g p =
  Set.fromList [ d
      | q <- Set.toList (peers p)
      , d <- (case lookG g q of Just d -> [d]; Nothing -> [])
      ]

peers :: Pos -> Set Pos
peers p = hor p `union` vert p `union` box p

hor :: Pos -> Set Pos
hor Pos{y} = Set.fromList [Pos{x,y} | x <- [1..9] ]

vert :: Pos -> Set Pos
vert Pos{x} = Set.fromList [Pos{x,y} | y <- [1..9] ]

box :: Pos -> Set Pos
box Pos{x=x0,y=y0} = do
  let x' = (x0-1) `div` 3
  let y' = (y0-1) `div` 3
  Set.fromList [Pos{x=3*x'+xo,y=3*y'+yo} | xo <- [1,2,3], yo <- [1,2,3] ]


newtype Grid = Grid (Map Pos Digit)

filled :: Grid -> Pos -> Bool
filled g p = case lookG g p of Just{} -> True; Nothing -> False

lookG :: Grid -> Pos -> Maybe Digit
lookG (Grid m) p = Map.lookup p m

extendG :: Grid -> (Pos,Digit) -> Grid
extendG (Grid m) (p,d) = Grid (Map.insert p d m)

instance Show Grid where
  show (Grid m) = do
    let
      look :: Pos -> String
      look p = case Map.lookup p m of Just d -> show d; Nothing -> "."
    concat
      [ concat [ look pos
               | x <- [1..9]
               , let pos = Pos {x,y}
               ] ++ "\n"
      | y <- [1..9]
      ]


data Pos = Pos { x, y :: Int } deriving (Eq,Ord)

instance Show Pos where show Pos{x,y} = show (x,y)

allPos :: [Pos]
allPos = [ Pos {x,y} | y <- [1..9], x <- [1..9] ]


newtype Digit = Digit Int deriving (Eq,Ord)

mkDigit :: Char -> Digit
mkDigit c = do
  let ord0 = ord '0'
  let n = ord c - ord0
  if n<1 || n>9 then error (show ("mkDigit",c,n)) else Digit n

instance Show Digit where show (Digit d) = show d

allDigits :: Set Digit
allDigits = Set.fromList [ mkDigit n | n <- ['1' ..'9'] ]
