
## Haskell Homework #1

Hey Ellie,

For your Haskell homework, lets work up some more list processing
functions... and end with computing pythagorean triples!

I've setup the cloud repl for you:
https://replit.com/@NickChapman3/AdmiredAliveMultitasking#main.hs

Have fun,
Dad!

### Recap

So far we saw how we can implement our own version of some standard
list processing functions, having the following types:
```
  map :: (a -> b) -> [a] -> [b]
  filter :: (a -> Bool) -> [a] -> [a]
  length :: [a] -> Int
  reverse :: [a] -> [a]
  append :: [a] -> [a] -> [a]
```
(Note the standard version of `append` is written as the infix operator `++`.)

We are going to continue by implementing some more standard functions:
`zip`, `take`, `drop` and `concat`
You will need the following to avoid a clash with the Prelude version:
```
  import Prelude hiding (zip,take,drop,concat)
```

#### (1) `upto`

But lets begin with `upto`, having the following type:
```
  upto :: Int -> Int -> [Int]
```
This function should generate the list of ints between a start and stop value.
So for example:
```
  print (upto 10 20)
  -- [10,11,12,13,14,15,16,17,18,19,20]

  print (upto 20 10)
  -- []
```

#### (2) `take` and `drop`.

These take a number(N) and a list, and return a list.
- `take` returns the initial N elements.
- `drop` returns everything but the initial N elements.

Types are:
```
  take :: Int -> [a] -> [a]
  drop :: Int -> [a] -> [a]
```

Example use:
```
  print (take 5 (upto 100 10000000))
  -- [100,101,102,103,104]

  print (take 5 (drop 10 (upto 1 10000000)))
  -- [11,12,13,14,15]
```

#### (3) `zip` takes two lists, and returns a list of pairs.

Note:
- A pair is a tuple of size 2. Haskell allows tuples of any fixed size.
- A pair (or any tuple) is constructed using parentheses and commas.
- For example: `let v = (True,"hello")`
- `v` has type `(Bool,String)`

(Note the type is also written with parentheses and commas.)
The type we want for zip is:
```
  zip :: [a] -> [b] -> [(a,b)]
```
Example use:
```
  print (zip (upto 1 20) ["fe","fi","fo","fum"])
  -- [(1,"fe"),(2,"fi"),(3,"fo"),(4,"fum")]
```

Notice how `zip` returns a list which has the same length as the
shorter of the two input lists.


#### (4) Concatenation. Flatten a list of lists into a single list.

Type:
```
  concat :: [[a]] -> [a]
```
Example:
```
  print (concat [ upto 1 3, upto 200 100, upto 7 8, upto 15 18 ])
  -- [1,2,3,7,8,15,16,17,18]
```

#### (5) Cartesian product: `cart`

Takes two lists. Returns a list of all possible pairs.
This has the same type as `zip`, but different behaviour.

The definition of `cart` is a little more tricky than the previous questions.
But you do have enough in your arsenal to write it!

Type:
```
  cart :: [a] -> [b] -> [(a,b)]
```
Example:
```
  print (cart (upto 1 3) ["fe","fi","fo","fum"])
  -- [(1,"fe"),(1,"fi"),(1,"fo"),(1,"fum"),(2,"fe"),(2,"fi"),(2,"fo"),(2,"fum"),(3,"fe"),(3,"fi"),(3,"fo"),(3,"fum")]
```

If you are having problems, perhaps first try to define a helper function:
with type:
```
    cartHelper :: [b] -> a -> [(a,b)]
```
and behaviour:
```
  print (cartHelper ["fe","fi","fo","fum"] 'X')
  -- [('X',"fe"),('X',"fi"),('X',"fo"),('X',"fum")]
```
and then perhaps you can use `cartHelper` is the definition of `cart`

#### (6) Now the payoff -- Pythagorean triples!

Lets make our list utils do some real work...
Write a function
```
  pythag :: Int -> [(Int,Int,Int)]
```
which given an int (n) returns the list of all pythagorean triples (x,y,z)
- such that: (x <= y <= z <= n)`
- and x^2 + y^2 = z^2

So:
```
  print (pythag 15)
  -- [(3,4,5),(5,12,13),(6,8,10),(9,12,15)]
```

You should have just enough Haskell _foo_ to be able to write this.
I wont give you any more clues here.
But ask me if you get stuck.
