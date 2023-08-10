
module ParserCombinators where --(main) where

import qualified Data.Char as Char (ord)

main :: IO ()
main = do
  let _ = print (parseJson "[123,true,null]")

  test lower "" Nothing
  test lower "456xyz" Nothing
  test lower "foo123" (Just ('f',"oo123"))

  test digit "" Nothing
  test digit "foo123" Nothing
  test digit "456xyz" (Just (4,"56xyz"))

  test lowerThenDigit "" Nothing
  test lowerThenDigit "x" Nothing
  test lowerThenDigit "xy" Nothing
  test lowerThenDigit "x1y" (Just (('x',1),"y"))
  test lowerThenDigit "1xy" Nothing

  test twoDigitNumber "" Nothing
  test twoDigitNumber "2" Nothing
  test twoDigitNumber "23" (Just (23,""))
  test twoDigitNumber "234" (Just (23,"4"))

  test lowerOrDigit "" Nothing
  test lowerOrDigit "xy89" (Just (Left 'x', "y89"))
  test lowerOrDigit "89xy" (Just (Right 8, "9xy"))
  test lowerOrDigit "WHAT" Nothing



test :: (Eq a,Show a) => Parser a -> String -> Maybe (a,String) -> IO ()
test p input expected = do
  print (expect (parse p input) expected)


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


lower :: Parser Char
lower = satisfy (\x -> x >= 'a' && x <= 'z')

digit :: Parser Int
digit = mapP numOfDigitAscii (satisfy (\x -> x >= '0' && x <= '9'))
  where
    numOfDigitAscii :: Char -> Int
    numOfDigitAscii x = Char.ord x - Char.ord '0'

lowerThenDigit :: Parser (Char,Int)
lowerThenDigit = lower `sequenceP` digit

twoDigitNumber :: Parser Int
twoDigitNumber =
  mapP (\(d1,d2) -> 10*d1 + d2) (digit `sequenceP` digit)

lowerOrDigit :: Parser (Either Char Int)
lowerOrDigit =
  mapP Left lower
  `altP`
  mapP Right digit



json :: Parser Json
json = alts
  [ mapP JNumber number
  , mapP JString string
  , mapP (\() -> JNull) jnull
  ]

number :: Parser Int
number = undefined

string :: Parser String
string = undefined

jnull :: Parser ()
jnull = undefined



----------------------------------------------------------------------

alts :: [Parser a] -> Parser a
alts ps = case ps of
  [] -> failP
  p:ps -> p `altP` alts ps


data Parser a = Parser (String -> Maybe (a,String))


parse :: Parser a -> String -> Maybe (a,String)
parse (Parser f) input = f input

failP :: Parser a
failP = Parser (\_ -> Nothing)

altP :: Parser a -> Parser a -> Parser a
altP (Parser fa1) (Parser fa2) =
  Parser (\s ->
            case fa1 s of
              Just (x,s) -> Just (x,s)
              Nothing ->
                case fa2 s of
                  Just (x,s) -> Just (x,s)
                  Nothing -> Nothing)

sequenceP :: Parser a -> Parser b -> Parser (a,b)
sequenceP (Parser fa) (Parser fb) =
  Parser (\s -> case fa s of
             Nothing -> Nothing
             Just (a,s) ->
               case fb s of
                 Nothing -> Nothing
                 Just (b,s) -> Just ((a,b),s))

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred =
  Parser (\s -> case s of
             "" -> Nothing
             x:xs -> if pred x then Just (x,xs) else Nothing)

mapP :: (a -> b) -> Parser a -> Parser b
mapP f (Parser pa) =
  Parser (\s -> case (pa s) of
             Nothing -> Nothing
             Just (a,s) -> Just (f a, s)
            )
