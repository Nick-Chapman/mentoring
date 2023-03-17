
## Homework 8

Extend the brainfuck parser to support `[` and `]`.

Extend the brainfuck interpreter to support output (`.`) and input (`,`).

Add `Dot` and `Comma` to the `Op` type.
```
data Op
  = Plus
  | Minus
  | Rarrow
  | Larrow
  | Block [Op]
  | Dot
  | Comma
```

### Output

Here are some programs which don't do any input...

- [collatz](https://github.com/Nick-Chapman/bf/blob/master/b/collatz.b) prints the collatz sequence starting from 27 and then stops.
- [fib](https://github.com/Nick-Chapman/bf/blob/master/b/fib.b) prints the (infinite) Fibonacci sequence and doesn't terminate.
- [mandelbrot](https://github.com/Nick-Chapman/bf/blob/master/b/mandelbrot.b) prints a textual picture of the mandelbrot set. It does stop, but it is very compute intensive. You probably want to add `- -O2` to the `ghc-options:` in `package.yaml`. You can expect a couple of seconds for each line printed.

### Tips...

Currently the result from the interpreter is `[Tape]`, which we display using `mapM_ print ...` With a tiny bit of refactoring we could write this as:
```
type Result = [Tape]
seeResult :: Result -> IO ()
seeResult tapes = mapM_ print (zip [0::Int ..] tapes)
```

To support output we need to change the `Result` type. Perhaps we could pair each `Tape` in the output list with an optional `Word8` to represent output which occurred on that step.

```
type Result = [(Tape,Maybe Word8)]
```

This could work, but it's somewhat clunky. I think things work out better with the following type...

```
data Result = Finish | Step Tape Result | Output Word8 Result
```

The first two constructors (`Finish` and `Step`) take the place of _nil_ and _cons_ when we were using a list. But now we have an extra constructor `Output` for when we encounter the `Dot` op.

This type for `Result` is also a better base for adding input... but we can do this together on the next session.


### Input

Here are some programs which do some input:

- [rev](https://github.com/Nick-Chapman/bf/blob/master/b/rev.b)
- [triplicate](https://github.com/Nick-Chapman/bf/blob/master/b/triplicate.b)
- [factor](https://github.com/Nick-Chapman/bf/blob/master/b/factor.b)

The last program finds the prime factors of its input:
```
echo '12345' | stack run ~/code/bf/b/factor.b
```
