module Top (main) where

import qualified Coins (playCoins)

main :: IO ()
main = do
  putStrLn "*h101*"
  Coins.playCoins
