
module PC2 where

import qualified Data.Char as Char (ord)

main :: IO ()
main = do
  print "xx"

  test lower "" Nothing
  test lower "a" (Just 'a')
  test lower "A" Nothing
  test lower "ab" Nothing

  test digit "" Nothing
  test digit "1" (Just 1)
  test digit "A" Nothing
  test digit "12" Nothing

  test lowerThenDigit "" Nothing
  test lowerThenDigit "x" Nothing
  test lowerThenDigit "xy" Nothing
  test lowerThenDigit "x1" (Just ('x',1))
  test lowerThenDigit "x1y" Nothing
  test lowerThenDigit "1x" Nothing

  test lowerOrDigit "" Nothing
  test lowerOrDigit "x" (Just (Left 'x'))
  test lowerOrDigit "8" (Just (Right 8))
  test lowerOrDigit "A" Nothing
  test lowerOrDigit "x8" Nothing

  test number2 "" Nothing
  test number2 "x" Nothing
  test number2 "1" (Just 1)
  test number2 "12" (Just 12)
  test number2 "12x" Nothing
  test number2 "123" Nothing

  test number "" Nothing
  test number "x" Nothing
  test number "1" (Just 1)
  test number "12" (Just 12)
  test number "12x" Nothing
  test number "123" (Just 123)
  test number "1234" (Just 1234)



test :: (Eq a,Show a) => Parser a -> String -> Maybe a -> IO ()
test p input expected = do
  print (expect (parse p input) expected)

expect :: (Eq a, Show a) => a -> a -> a
expect a b = if a == b then a else
  error ("expect failed: " ++ show a ++ " not same as: " ++ show b)


lower :: Parser Char
digit :: Parser Int
lowerThenDigit :: Parser (Char,Int)
lowerOrDigit :: Parser (Either Char Int)


lower = satisfyP (\x -> x >= 'a' && x <= 'z')

digit = mapP convert (satisfyP (\x -> x >= '0' && x <= '9'))
  where convert x = Char.ord x - Char.ord '0'

lowerThenDigit = lower `sequenceP` digit

lowerOrDigit = mapP Left lower `altP` mapP Right digit


number2 :: Parser Int
number2 =
  (mapP (\(a,b) -> 10*a+b) (digit `sequenceP` digit))
  `altP`
  digit


number :: Parser Int

{-number =
  digit
  `altP`
  mapP (\(a,b) -> a*10+b) (number `sequenceP` digit)
-}


number = mapP (collapse 0) (someP digit)
  where
    collapse acc = \case
      [] -> acc
      d:ds -> collapse (10*acc+d) ds

manyP :: Parser a -> Parser [a]
someP :: Parser a -> Parser [a]

someP p = mapP (\(x,xs) -> x:xs) (p `sequenceP` manyP p)
manyP p = someP p `altP` (returnP [])



data Parser a = Parser (String -> Maybe (a,String))

parse :: Parser a -> String -> Maybe a

returnP :: a -> Parser a
failP :: Parser a
altP :: Parser a -> Parser a -> Parser a
sequenceP :: Parser a -> Parser b -> Parser (a,b)
satisfyP :: (Char -> Bool) -> Parser Char
mapP :: (a -> b) -> Parser a -> Parser b

----------------------------------------------------------------------

returnP x = Parser (\s -> Just (x,s))
failP = Parser (\_ -> Nothing)

parse (Parser f) input =
  case (f input) of
    Nothing -> Nothing
    Just (x,s) ->
      case s of
        "" -> Just x
        _ -> Nothing

satisfyP pred =
  Parser (\s -> case s of
             "" -> Nothing
             x:xs -> if pred x then Just (x,xs) else Nothing)

mapP f (Parser pa) =
  Parser (\s -> case (pa s) of
             Nothing -> Nothing
             Just (a,s) -> Just (f a, s)
            )

--sequenceP (Parser fa) (Parser fb) =
sequenceP (Parser fa) pb =
  Parser (\s -> case fa s of
             Nothing -> Nothing
             Just (a,s) -> do
               let Parser fb = pb
               case fb s of
                 Nothing -> Nothing
                 Just (b,s) -> Just ((a,b),s))

--altP (Parser fa1) (Parser fa2) =
altP (Parser fa1) pa2 =
  Parser (\s ->
            case fa1 s of
              Just (x,s) -> Just (x,s)
              Nothing -> do
                let Parser fa2 = pa2
                case fa2 s of
                  Just (x,s) -> Just (x,s)
                  Nothing -> Nothing)

----------------------------------------------------------------------
{-
  test dquote "" Nothing
  test dquote "\"" (Just ())
  test dquote "A" Nothing
  test dquote "\"text\"" Nothing

  test notDquote "" Nothing
  test notDquote "A" (Just 'A')
  test notDquote "\"" Nothing
  test notDquote "AB" Nothing
-}

--lit :: Char -> Parser ()
--dquote :: Parser ()
--notDquote :: Parser Char
--string :: Parser String

{-dquote = lit '\"'
lit c = mapP (\_ -> ()) (satisfyP (\x -> x == c))
notDquote = satisfyP (\x -> x /= '\"')
-}

--(string) = undefined

--number = digit `altP` (mapP (\(a,b) -> 10*a+b) (digit `sequenceP` number))
