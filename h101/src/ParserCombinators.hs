
module ParserCombinators (main) where

import qualified Data.Char as Char (ord)

main :: IO ()
main = do
  let _ = print (parseJson "[123,true,null]")

  print (expect (parseLower "") Nothing)
  print (expect (parseLower "456xyz") Nothing)
  print (expect (parseLower "foo123") (Just ('f',"oo123")))

  print (expect (parseDigit "") Nothing)
  print (expect (parseDigit "foo123") Nothing)
  print (expect (parseDigit "456xyz") (Just (4,"56xyz")))

  print (expect (parseLowerThenDigit "") Nothing)
  print (expect (parseLowerThenDigit "x") Nothing)
  print (expect (parseLowerThenDigit "xy") Nothing)
  print (expect (parseLowerThenDigit "x1y") (Just (('x',1),"y")))
  print (expect (parseLowerThenDigit "1xy") Nothing)

  print (expect (parseTwoDigitNumber "") Nothing)
  print (expect (parseTwoDigitNumber "2") Nothing)
  print (expect (parseTwoDigitNumber "23") (Just (23,"")))
  print (expect (parseTwoDigitNumber "234") (Just (23,"4")))

  print (expect (parseLowerOrDigit "") Nothing)
  print (expect (parseLowerOrDigit "xy89") (Just (Left 'x', "y89")))
  print (expect (parseLowerOrDigit "89xy") (Just (Right 8, "9xy")))
  print (expect (parseLowerOrDigit "WHAT") Nothing)


expect :: (Eq a, Show a) => a -> a -> a
expect a b = if a == b then a else
  error ("expect failed: " ++ show a ++ " not same as: " ++ show b)


parseJson :: String -> Maybe Json
parseJson = undefined

data Json
  = JObject [(String,Json)]
  | JArray [Json]
  | JString String
  | JNumber Int
  | JTrue
  | JFalse
  | JNull
  deriving Show


parseLower :: String -> Maybe (Char,String)
parseDigit :: String -> Maybe (Int,String)

parseLowerThenDigit :: String -> Maybe ((Char,Int),String)
parseTwoDigitNumber :: String -> Maybe (Int,String)

parseLowerOrDigit :: String -> Maybe (Either Char Int, String)

parseLower = \case
  "" -> Nothing
  x:xs -> if x >= 'a' && x <= 'z' then Just (x,xs) else Nothing

parseDigit = \case
  "" -> Nothing
  x:xs -> if x >= '0' && x <= '9' then Just (num,xs) else Nothing
    where num = Char.ord x - Char.ord '0'

parseLowerThenDigit s =
  case parseLower s of
    Nothing -> Nothing
    Just (x,s) ->
      case parseDigit s of
        Nothing -> Nothing
        Just (d,s) -> Just ((x,d),s)


parseTwoDigitNumber s =
  case parseDigit s of
    Nothing -> Nothing
    Just (d1,s) ->
      case parseDigit s of
        Nothing -> Nothing
        Just (d2,s) -> Just ( 10*d1 + d2, s)

parseLowerOrDigit s =
  case parseLower s of
    Just (x,s) -> Just (Left x,s)
    Nothing ->
      case parseDigit s of
        Just (d,s) -> Just (Right d,s)
        Nothing -> Nothing
