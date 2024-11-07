
Hey Ellie,

## Homework 11

In the last session we consolidated our understanding of the Emitter monad (Also known as the Output monad). Here we focus on the State Monad.

### State Monad

The state monad (`SM a`) manages a single piece of state during a computation. It provides means to `get` the current value of the state, and to `set` the state to a new value. As for any monad, there will be `bind` and `return` operators. And there must be a way to `run` the monadic computation.

For this example we shall assume the state is always just a single `Int`. The semantics of `run` will initialize the state with `1` and discard the final value of the state. We'll use the suffix `SM` consistently on the operators. So we have five operations with the following types:
```
data SM a -- = ???

getSM :: SM Int
setSM :: Int -> SM ()
returnSM :: a -> SM a
bindSM :: SM a -> (a -> SM b) -> SM b
runSM :: SM a -> a
```

### Labelling a Tree

To motivate the use of this monad, imagine the task of labelling the leafs of a binary tree with consecutive integers.

First, lets define the tree type, give it a `Show` instance, and make a small example:
```
data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show = \case
    Leaf a -> show a
    Node t1 t2 -> "(" ++ show t1 ++ "," ++ show t2 ++ ")"

tree1 :: Tree String
tree1 = Node (Node (Leaf "a") (Leaf "b")) (Node (Leaf "c") (Leaf "d"))
```

Now we want to define a labelling function with the following type:
```
label :: Tree a -> SM (Tree (a,Int))
label = undefined -- CODE ME
```
The idea is that this function (working in the state monad) will traverse a given tree, constructing a new tree of the same shape, but with every leaf replaced by a leaf containing the original value (of type `a`) with a pair of that value and a consecutive integer.

To be concrete, from the following driving code:
```
main :: IO ()
main = do
  print tree1
  let t2 = runSM (label tree1)
  print t2
```
the expected output is:
```
(("a","b"),("c","d"))
((("a",1),("b",2)),(("c",3),("d",4)))
```

### Task 1
Implement `label`. This should be quite straightforward. Pattern match on the input tree. In the `Node` case, recurse to the left and right & reconstruct a node. In the `Leaf` case, obtain the next consecutive integer, and pair it with the input leaf value. The function needs to be coded _in_ the state monad, as the required type indicates, and so use of the `do` syntax will helpful.

In the leaf case, you might want to define an auxiliary helper function, `tick` which captures and combines two steps (i) obtain the current integer-state from the state-monad, (ii) set the integer-state to be one greater than it was.
```
tick :: SM Int
tick = undefined -- CODE ME using getSM and setSM
```

Make sure your code for `label` and `tick` type-checks. (But you wont be able to run the example until the `SM` monad operators are also defined). As usual, to make use of the `do` syntax, you will need the Haskell magic to declare that `SM` is a monad, here defined by `returnSM` and `bindSM`.
```
import Control.Monad (ap,liftM) -- imports must git at the start of the file.

instance Functor SM where fmap = liftM
instance Applicative SM where pure = returnSM; (<*>) = ap
instance Monad SM where (>>=) = bindSM
```

### Task 2 & Task 3

Implement the five SM operators:
```
getSM :: SM Int
getSM = undefined

setSM :: Int -> SM ()
setSM = undefined

returnSM :: a -> SM a
returnSM = undefined

runSM :: SM a -> a
runSM = undefined

bindSM :: SM a -> (a -> SM b) -> SM b
bindSM = undefined
```

Make this implementation in both of the styles we talked about -- GADT and standard -- You can attempt this in either order. (I'm so kind). Note, to typecheck and run the code, only one style can be defined at a time, the other can be commented out. As a headstart, I'll give you the two alterative definitions for `SM`. Feel free to try to figure this out yourself, but no shame to make use of this.


### Task 2; Standard style
Define `SM` as a type which captures the semantics of the state monad:
```
data SM a = SM (Int -> (a,Int))
```

You can read this as: An `SM` is a function from the current value of the integer state, to a pair of the `a` value carried by the monad, plus a changed value of the integer state.


### Task 3; GADT style
Define `SM` as a type capturing the ways that an `SM` can be constructed:
```
data SM a where
  Ret :: a -> SM a
  Bind :: SM a -> (a -> SM b) -> SM b
  Get :: SM Int
  Set :: Int -> SM ()
```

Note that when working on the GADT style implementation, the `runSM` function will contain all of the complexity. And, THIS IS IMPORTANT, although the type for `runSM` is simply `SM a -> a `, you will need to implement an inner recursive function with a richer type: `innerRun :: SM a -> Int -> (a,Int)`.


### (Bonus) Task 4; Really really easy.

Make a tiny change to the implementation of your `label` function so that the tree is labelled from right to left: Now the expected output should be:
```
(("a","b"),("c","d"))
((("a",4),("b",3)),(("c",2),("d",1)))
```

Good luck. Grab me if you need any help.


Dad
xxx
