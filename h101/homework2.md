
(1) A couple of old AoC questions be attempt in Haskell

https://adventofcode.com/2020/day/1

You will find you need a way to convert from `String` to `Int`
i.e. `"42"` --> `42`
And the standard `read` function will help you here.
It's kind of like an inverse of `show`, and like `show` it works for different types (adhoc polymorphism), using Haskell's type-class mechanism.

As the types will reveal..
```
show :: Show a => a -> String
read :: Read a => String -> a
```

(`Read` is the type-class. which says a type supports `read`ing from a string)
Now you know: `Show`, `Eq`, `Ord` and `Read` :)

Using read will feel like magic... Just pass it a string, and the correct overload will be selected as deterined by the type demanded by the context, say `Int`.


https://adventofcode.com/2019/day/4

This is the question I was looking for: lots of fun using list comprehensions!



(2) Think about the types and functions which will be useful to implement
a brainfuck interpreter:

https://en.wikipedia.org/wiki/Brainfuck

Including:
- the type for the toplevel function which runs a program and produces output
- the types for programs and memory-tapes
- the functions which implement the primitive ops
- the functions which control the step-by-step operation of the interpreter
- the functions which will help us debug this thing!

Sometimes you might have an idea for how some types might be represented.
```
type Foo = (Int,Int) -- Foo is a pair of ints
```
or an enum such as
```
data Colour = Red | Green | Blue
```

Sometime you will just know that you need _some_ type, perhaps `Thing`
but are not sure what the representation will be yet.
Although you have an idea what functions will operate on that type.
(And this will help guide the representation!)

```
emptyThing :: Thing
throbThing :: Int -> Thing -> Thing
collapseThing :: Thing -> (Int,Bool)
```
