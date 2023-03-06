
## Homework 7

Motivating the state monad. First we must feel the pain...


### Funky Trees

Haskell makes it very easy to define datatypes for whatever shape out application needs. For example...

```
data FunkyTree a
  = FunkyBinNode (FunkyTree a) (FunkyTree a)
  | FunkyBinNodeWithData a (FunkyTree a) (FunkyTree a)
  | FunkyLeaf a
  | FunkyEmpty
  | Funky3Way (Maybe a) (FunkyTree a, FunkyTree a, FunkyTree a)
```

`FunkyTree` is parameterised by `a`, the type of data value contained by the tree.
These data values can appear at various places...

Simple leaves are represented by `FunkyLeaf`; every leaf carries a data value.

There are binary nodes of the form `FunkyBinNode`. Each of these nodes has two children, but carries no data value.

There is a separate constructor `FunkyBinNodeWithData`, which also has two children, but in addition does carry a data value.

There is also `FunkyEmpty` with no children and no data.

And then to mix things up there is a 3-way node `Funky3Way` with three children, and an optional data-value, represented using `Maybe a`.

(`Maybe` is a standard haskell datatype with the following definition: `data Maybe a = Nothing | Just a`)


Now we have our funky definition, lets define a sample value, and then think about some functions we should like which operate over `FunkyTree`.
```
  let
    funky1 :: FunkyTree Char =
      FunkyBinNode
      (FunkyBinNodeWithData 'a'
       (FunkyLeaf 'b')
        (Funky3Way Nothing (FunkyEmpty,FunkyEmpty,FunkyEmpty)))
      (Funky3Way (Just 'c')
       ( FunkyLeaf 'd'
       , FunkyEmpty
       , Funky3Way (Just 'e') (FunkyEmpty,FunkyEmpty,FunkyEmpty)
       ))
```

### Warmup...

Define a function `mapFunky` which _maps_ a given function of type `a -> b` over a `FunkyTree`

```
mapFunky :: (a -> b) -> FunkyTree a -> FunkyTree b
mapFunky = undefined
```

This function should be reminiscent of the standard function `map` which provides the same facility for lists.

Let's use `mapFunky` to map a simple `Char -> String` function over our `funky1` example. Expected result given.

```
  let funky2 = mapFunky (\c -> [c,c]) funky1
  let
    funky2expect :: FunkyTree String =
      FunkyBinNode
      (FunkyBinNodeWithData "aa"
       (FunkyLeaf "bb")
        (Funky3Way Nothing (FunkyEmpty,FunkyEmpty,FunkyEmpty)))
      (Funky3Way (Just "cc")
       ( FunkyLeaf "dd"
       , FunkyEmpty
       , Funky3Way (Just "ee") (FunkyEmpty,FunkyEmpty,FunkyEmpty)
       ))
```

(Aside. Haskell has some magic which allows the definition of functions like `mapFunky` to be completely automated -- now he tells me! -- This is because `FunkyTree` can be declared to be an instance of something called a `Functor`...but we save this fun for another day!)


### Main event...

Now for something a little bit trickier... not that tricky, just a bit painful, and ripe for bugs. The pain of doing this exercise is part of the motivation for a monadic based approach. We'll look at this on the next session.

Define a function `enumerateFunky` which labels every data value in a `FunkyTree` with a unique consecutive `Int` from 1 upto the number of data-items in the tree.

```
enumerateFunky :: FunkyTree a -> FunkyTree (Int,a)
enumerateFunky = undefined
```

The type is fairly instructive as to the required behaviour; but it does not say everything.

Additional requirements:
- Start the labelling from 1
- Children are numbered from left to right.
- Children are numbered before their parent.

Here's an example to be concrete:
```
  let funky3 = enumerateFunky funky2
  let
    funky3expect :: FunkyTree (Int,String) =
      FunkyBinNode
      (FunkyBinNodeWithData (2,"aa")
       (FunkyLeaf (1,"bb"))
        (Funky3Way Nothing (FunkyEmpty,FunkyEmpty,FunkyEmpty)))
      (Funky3Way (Just (5,"cc"))
       ( FunkyLeaf (3,"dd")
       , FunkyEmpty
       , Funky3Way (Just (4,"ee")) (FunkyEmpty,FunkyEmpty,FunkyEmpty)
       ))
```

When you implement `enumerateFunky`, you will need to recurse over the input tree, but also pass along a counter for each data item encountered in the tree. This counter will need to be passed down and up the traversal, and will be incremented for each data item. It's fiddly!

You might define a recursive helper function with the following type:
```
    trans :: Int -> FunkyTree a -> (Int, FunkyTree (Int,a))
```

This helper `trans` (short for _transform_) has type `Int ->` for the counter value going down the traversal, And `-> (Int, ... )` for the updated counter value, paired up with the main result; the labeled tree, coming up the traversal. If you nest this definition inside a where clause of `enumerateFunky` then having a short name is good style.
```
enumerateFunky :: forall a. FunkyTree a -> FunkyTree (Int,a)
enumerateFunky tree = undefined (trans,tree)
  where
    trans :: Int -> FunkyTree a -> (Int, FunkyTree (Int,a))
    trans = undefined
```

Don't be put out by the `forall a.` prefix added to the above type definition. It essentially means nothing here, but avoids an obscure haskell typing issue which we can discuss during our next session!

Or just make `trans` a top level definition if you prefer.
