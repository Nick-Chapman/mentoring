module Top (main) where

import qualified Coins (playCoins)
import qualified Bf (main)

main :: IO ()
main = do
  putStrLn "*h101*"
  let _ = Coins.playCoins
  Bf.main
