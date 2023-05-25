
module Sol (main) where

import Control.Monad (when)
import Data.Set (Set)
import Text.Printf (printf)
import qualified Data.Set as Set
import Data.Bits
import Data.Word (Word64)

main :: IO ()
main = do
  putStrLn "*Solitaire*"
  searchTop >>= \case
    Fail n _bad ->
      printf "No solution found in %d steps\n" n
    Solution n moves -> do
      printf "Solution found in %d steps\n" n
      mapM_ (\(i,m) -> printf "(%d) %s\n" i (show m)) (zip [1::Int ..] moves)

data SearchResult
  = Solution Int [Move]
  | Fail Int (Set Board)
  deriving Show

searchTop :: IO SearchResult
searchTop = search 1 Set.empty 0 [] initBoard
  where
    search :: Int -> Set Board -> Int -> [Move] -> Board -> IO SearchResult
    search count bad depth acc board = do
      let win = isWin board
      --printf "%d: %s -- %s\n" count (show board) (if win then "WIN" else "no")
      let ms = movesOf board
      when (count `mod` 1000000 == 0) $ printf "%d: #bad=%d  depth=%d  %s  #moves=%d%s\n" count (Set.size bad) depth (show board) (length ms) (if win then " -- WIN" else "")
      if board `Set.member` bad then do --printf "BAD\n"
                                        pure (Fail count bad) else do
      if win then pure (Solution count (reverse acc)) else do
        loop count bad depth acc board 1 ms

    loop :: Int -> Set Board -> Int -> [Move] -> Board -> Int -> [Move] -> IO SearchResult
    loop count bad depth acc board i = \case
      [] -> pure (Fail count (Set.insert board bad))
      m:ms -> do
        --printf "%d: depth:%d, alt:%d : %s\n" count depth i (show m)
        res <- search (count+1) bad (depth+1) (m:acc) (applyMove board m)
        case res of
          Solution{} -> pure res
          Fail count bad ->
            loop count bad depth acc board (i+1) ms


movesOf :: Board -> [Move]
movesOf b = filter (isLegalMove b) allMoves

isLegalMove :: Board -> Move -> Bool
isLegalMove b m = case tryMove b m of Just{} -> True; Nothing -> False

applyMove :: Board -> Move -> Board
applyMove b m = case tryMove b m of Just b' -> b'; Nothing -> undefined

data Move = M { on1 :: Int, on2 :: Int, off :: Int }

instance Show Move where
  show M{on1,on2,off} = printf "%d/%d-->%d" on1 on2 off

{-
       00 01 02
       03 04 05
 06 07 08 09 10 11 12
 13 14 15 16 17 18 19
 20 21 22 23 24 25 26
       27 28 29
       30 31 32
-}

allMoves :: [Move]
allMoves = all
  where
    all = oneDirection ++ [ M c b a | M a b c <- oneDirection ]
    oneDirection = h ++ v
    h =
      [                         M  0  1  2
      ,                         M  3  4  5
      , M  6  7  8, M  7  8  9, M  8  9 10, M  9 10 11, M 10 11 12
      , M 13 14 15, M 14 15 16, M 15 16 17, M 16 17 18, M 17 18 19
      , M 20 21 22, M 21 22 23, M 22 23 24, M 23 24 25, M 24 25 26
      ,                         M 27 28 29
      ,                         M 30 31 32
      ]
    v =
      [                         M  6 13 20
      ,                         M  7 14 21
      , M  0  3  8, M  3  8 15, M  8 15 22, M 15 22 27, M 22 27 30
      , M  1  4  9, M  4  9 16, M  9 16 23, M 16 23 28, M 23 28 31
      , M  2  5 10, M  5 10 17, M 10 17 24, M 17 24 29, M 24 29 32
      ,                         M 11 18 25
      ,                         M 12 19 26
      ]


initBoard :: Board
--initBoard = makeBoard [0,3,4,7,8,9,10] --7 marbles
--initBoard = makeBoard [0,3,4,7,8,9,10,32] --7+1 marbles (no solution
initBoard = makeBoard ([0.. 15] ++ [17..32]) -- all 32 marbles
--initBoard = makeBoard ([0.. 15] ++ [17..31]) -- missing corner marble


-- bit-based rep: solve in 0.25

newtype Board = B Word64 deriving (Eq,Ord)
instance Show Board where show (B w) = printf "%09x" w
makeBoard :: [Int] -> Board
makeBoard xs = B $ foldl (.|.) 0 [ 1 `shiftL` x | x <- xs ]
isWin :: Board -> Bool
isWin (B state) = state == center
  where center = 1 `shiftL` 16
tryMove :: Board -> Move -> Maybe Board
tryMove (B state) M{on1,on2,off} = if legal then Just (B updated) else Nothing
  where
    legal = isOn on1 && isOn on2 && not (isOn off)
    updated = state `clearBit` on1 `clearBit` on2 `setBit` off
    isOn = (state `testBit`)


-- original rep: solve in 1.8s
{-
newtype Board = B (Set Int) deriving (Eq,Ord)
instance Show Board where show (B set) = show (Set.toList set)
makeBoard :: [Int] -> Board
makeBoard xs = B (Set.fromList xs)
tryMove :: Board -> Move -> Maybe Board
tryMove (B set) M{on1,on2,off} =
  if Set.member on1 set && Set.member on2 set && not (Set.member off set)
  then Just (B (Set.insert off (Set.delete on1 (Set.delete on2 set))))
  else Nothing
isWin :: Board -> Bool
isWin (B set) = set == Set.singleton 16
-}
