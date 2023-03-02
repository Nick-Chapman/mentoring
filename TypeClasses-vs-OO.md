
### TypeClasses vs OO

This short note compares the difference in approach to Adhoc polymorphism taken by OO and TypeClass based systems. One important aspect which helps my intuitive understanding of how they compare is the difference in runtime representations they use to enable type-based dispatch.

#### OO

In the OO approach, *each object* carries a reference to its vtbl, which is used to implement the virtual dispatch.

So given the classic example of a `Shape` class with a computeArea() method, and classes `Circle` and `Square` which subtype or implement `Shape`, we achieve type based selection by writing `myShape->computeArea()`.

The vtbl referenced by `myShape` is used to select the `computeArea()` method appropriate for the type of `myShape`.

#### Typeclasses

In contrast, the TypeClass approach has the idea of a runtime dictionary of functions which implement the TypeClass for a specific instance type. So, for example, given a typeclass `Ord`:
```
class Ord a where
  (<=) :: a -> a -> Bool
```
which provides an overloaded comparison operator `<=`, we might implement a `sort` function with the following type:
```
sort :: Ord a => [a] -> [a]
sort xs = ... let someX .. let anotherX .. if (someX <= anotherX) then ...
```

Behind the scenes, the type class constraint gets transformed into code which passes a dictionary parameter (to the `sort` function) providing the specific implementation of `<=` for the type `a`.

The important distinction between the OO and Typeclass approaches is that with TypeClasses, the dictionary values for `Ord a` exist and are passed *independently* from the values of type `a`, rather than each value of `a` having access to its own, potentially different, vtbl.

This approach works really nicely for typeclass examples such as `Ord`, where the overloadeded function `<=` takes more than one argument of type `a`, because we can be sure by construction that the *same* dictionary is used for all `a`s everywhere. (Not so for OO, where it is somewhat of a problem to use virtual dispatch for binary or multi-arg methods).

Finally, the typeclass approach allows overloaded methods which are indexed by their return type. The classic example of this in haskell is the `Read` typeclass, which provides an overloaded function to parse a value from a String (think `sscanf`):

```
class Read a where
  read :: String -> a
```

A somewhat contrived example of how this can be used looks like:
```
let someString :: String = ...
let (a,b) = read someString
let result :: Int = a + b
```

Here the type constraints of the program context select the correct instance of `read`, at type `(Int,Int)`, ensuring the correct dictionary value is passed to the overloaded `read` function.

As far as I know, this _return-type_ based selection just does not work for the OO approach.
