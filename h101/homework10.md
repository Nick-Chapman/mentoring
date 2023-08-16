
## Homework 10

Parser combinators

### Remembering what we did before...

Using the following implementation of `Parser`:
```
data Parser a = Parser (String -> Maybe (a,String))
```

Code combinators with the following types:
```
returnP :: a -> Parser a
failP :: Parser a
altP :: Parser a -> Parser a -> Parser a
sequenceP :: Parser a -> Parser b -> Parser (a,b)
satisfyP :: (Char -> Bool) -> Parser Char
mapP :: (a -> b) -> Parser a -> Parser b
```

(We did all of these in the last session, except `returnP` -- this is the always successful parser, which consumes no input, and produces the value given as argument)


Use the combinators to define the following simple parsers:
```
lower :: Parser Char
digit :: Parser Int
lowerThenDigit :: Parser (Char,Int)
lowerOrDigit :: Parser (Either Char Int)
```

Implement the following function to _run_ a parser:
```
parse :: Parser a -> String -> Maybe a
```

This is slightly changed from the session. We no longer return left over text after the entire parser has finished. Instead we insist that the entire input text is consumed.

This is clarified by the following tests:

```
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
```


Here is the new definition of `test`:
```
test :: (Eq a,Show a) => Parser a -> String -> Maybe a -> IO ()
test p input expected = do
  print (expect (parse p input) expected)

expect :: (Eq a, Show a) => a -> a -> a
expect a b = if a == b then a else
  error ("expect failed: " ++ show a ++ " not same as: " ++ show b)
```

### Number (1 or 2 digits)


Define a parser which allows 1 or 2 digit numbers...

```
number2 :: Parser Int
```

Test with
```
  test number2 "" Nothing
  test number2 "x" Nothing
  test number2 "1" (Just 1)
  test number2 "12" (Just 12)
  test number2 "12x" Nothing
  test number2 "123" Nothing
```

### Number (any number of digits)

Define a parser which allows any number of digits. Has same type as `number2`
```
number :: Parser Int
```

I expect you will have trouble making this work.

Tests:
```
  test number "" Nothing
  test number "x" Nothing
  test number "1" (Just 1)
  test number "12" (Just 12)
  test number "12x" Nothing
  test number "123" (Just 123)
  test number "1234" (Just 1234)
```
