
## Homework 5

### (1)

Given the Haskell definition of `thrice`
```
thrice f x = f (f (f x))
inc y = y+1
```
Or equivalently:
```
thrice = \f -> \x -> f (f (f x))
inc = \y -> y+1
```
Ponder the evaluation of `thrice thrice inc 0`

Perhaps work through (on paper) how the evaluation might proceed using _equational reasoning_
- by replacing (as needed) `thrice` and `inc` by their bodies.
- and/or using beta-reduction (see below)


Perhaps starting...
```
(thrice thrice inc) 0
-->
(thrice (thrice (thrice inc))) 0
-->
```

Hmm. How might we proceed? Perhaps we take the sub expression `(thrice inc)`
and replace with: `\x -> inc (inc (inc x)`

Sometimes you might reach an expression of the form:
```
(\x -> ...x...FOO...x...) (...BAR...)
```
(which is a lambda-expression, applied to an argument-expression)

This is reduced by replacing the argument (`...BAR...`) for every instance of the bound variable `x` in the RHS of the lambda-expression (`...x...FOO...x...`)

So for this example:
```
(\x -> ...x...FOO...x...) (...BAR...)
-->
...(...BAR...)...FOO...(...BAR...)...
```
This is called _beta-reduction_

And then finally, you will reach sub-expressions where the `+` gets evaluated
```
3+1
--->
4
```

If you do try this on paper, don't be stingy with parenthesis!


### (2)

Ponder the _Applied-Lambda_ emulation of _Let_ as captured in our interpreter as:
```
mkLet :: Identifier -> Exp -> Exp -> Exp
mkLet x rhs body = App (Lam x body) rhs
```

### (3)

Think about the types and functions which will be useful to implement
a brainfuck interpreter: See [here](homework2.md)

### (4) Bonus

Something completely different! _6502_

Have a little read/play here: [Easy 6502](https://skilldrick.github.io/easy6502/).
