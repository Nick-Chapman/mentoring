module Coins (playCoins) where

playCoins :: IO ()
playCoins = do
  putStrLn "*coins*"
  --let coins = SomeCoins FiftyP (SomeCoins Tuppence NoCoins)
  --let coins = Cons FiftyP (Cons Tuppence (Cons FiftyP Nil))
  --let coins = FiftyP:(Tuppence:(FiftyP:[]))
  let coins = [FiftyP,Tuppence,FiftyP]
  let n = coinsWorth coins
  putStrLn ("mycoin: (" ++ show coins ++ ") is worth: " ++ show n)


--data [a] = [] | a : [a]
--  deriving Show

--data MyList a = Nil | Cons a (MyList a)
--  deriving Show

coinsWorth :: [Coin] -> Int
{-coinsWorth coins =
  case coins of
    [] -> 0
    c1:cmore -> coinWorth c1 + coinsWorth cmore-}
--coinsWorth coins = sum (myMap (coinWorth,coins))
--coinsWorth coins = sum (myMap coinWorth coins)
coinsWorth coins = sum (map coinWorth coins)


--mapCoinWorth :: [Coin] -> [Int]
{-mapCoinWorth coins =
  case coins of
    [] -> []
    c:cs -> coinWorth c : mapCoinWorth cs-}
--mapCoinWorth coins = myMap (coinWorth,coins)

{-
myMap :: (a -> b) -> [a] -> [b]
myMap f xs =
  case xs of
    [] -> []
    x:xs -> f x : myMap f xs
-}

{-sumInts :: [Int] -> Int
sumInts xs =
  case xs of
    [] -> 0
    x:xs -> x + sumInts xs-}


{-
coinsWorth :: Coins -> Int
coinsWorth = \case
  NoCoins -> 0
  --OneCoin c -> coinWorth c
  --TwoCoins c1 c2 -> coinWorth c1 + coinWorth c2
  SomeCoins c1 cmore -> coinWorth c1 + coinsWorth cmore

--data Coins = NoCoins | OneCoin Coin | TwoCoins Coin Coin
--  deriving Show

--data Coins = NoCoins | SomeCoins Coin Coins
--  deriving Show
-}

coinWorth :: Coin -> Int
coinWorth = \case
  OnePence -> 1
  Tuppence -> 2
  FiftyP -> 50
  Pound -> 100
  Cheque n -> n

data Coin = OnePence | Tuppence | FiftyP | Pound | Cheque Int
  --deriving Show

instance Show Coin where
  show = \case
    OnePence -> "1p"
    Tuppence -> "2p"
    FiftyP -> "50p"
    Pound -> "quid"
    Cheque n -> "cheque for " ++ show n ++ " pennies"
