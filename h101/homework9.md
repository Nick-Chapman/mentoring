
## Homework 9

Parsing again. This time in Haskell.

Discovering the technique known as combinator parsing.

We will use the concrete example of parsing Json syntax, to motivate both the use and implementation of parser combinators.


### Json

You are probably quite familiar with Json syntax. It is a lightweight human-readable data-interchange format. Here's a small example:
https://en.wikipedia.org/wiki/JSON#Syntax

You can find a specification of the syntax here.
https://www.json.org/json-en.html


We would like to define a haskell type `Json` to represent the abstract structure of Json, and the target of the Json parser we shall construct. This will have a type something like:

```
parseJson :: String -> Maybe Json
```

The `Maybe` type allows us to indicate that a string can be successfully parsed into a `Json` value (via the `Just` constructor), or cannot be parsed (via the `Nothing` constructor). If we wanted to indicate additional information in the case of a failed parse we might user a richer return type such as `Either ParseError Json` where `ParseError` contains info about where and why the parse failed.

Anyway. This is all just context. For Part 1, please give a definition of `Json` which can represent the different kinds of Json value (_object_, _array_, _string_, _number_, ...) -- check the syntax specification above.


### Basic parsing functions

Constructing a parser using combinators involves building up a parser out of smaller component parsers. The smallest parsers will deal with parsing the simplest items such as a single lowercase char or numeric digit from the head of the input string. For example, we might define parsing functions such as:

```
parseLower :: String -> Maybe (Char,String)
parseDigit :: String -> Maybe (Int,String)
```

These functions take an input string and attempt to parse (1) a lowercase char, or (2) a single digit, from the head of the string. In the case of a failed parse, `Nothing` should be returned. For a successful parse, the parsed item together with the remaining string is returned (wrapped in `Just`).

To be more concrete about the required behaviour, here are some examples:

```
  print (expect (parseLower "") Nothing)
  print (expect (parseLower "456xyz") Nothing)
  print (expect (parseLower "foo123") (Just ('f',"oo123")))

  print (expect (parseDigit "") Nothing)
  print (expect (parseDigit "foo123") Nothing)
  print (expect (parseDigit "456xyz") (Just (4,"56xyz")))
```

Here is the definition for expect:
```
expect :: (Eq a, Show a) => a -> a -> a
expect a b = if a == b then a else
  error ("expect failed: " ++ show a ++ " not same as: " ++ show b)
```

### Composing parsing functions

Now we have some basic parsing functions, we can use them to define some slightly less simple parsing functions:

```
parseLowerThenDigit :: String -> Maybe ((Char,Int),String)
parseTwoDigitNumber :: String -> Maybe (Int,String)
parseLowerOrDigit :: String -> Maybe (Either Char Int, String)
```

In case the name is not descriptive enough, here are some examples:
```
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
```

Hey. You guessed it. Please code these functions.


### Parsers and Parser combinators

On our coming session, we will use your implementations as a starting point to motivate the idea of parser combinators: their use and implementation.

Dad xxx
