
## Support for Lam/App

### Recap:

We added support for Lambda-expressions and Application-expressions to our expression type.

```
data Exp =
  ...
  | Lam Identifier Exp
  | App Exp Exp
```

We extended our `eval` function for the new cases, dispatching to `apply` for the application-case.

```
eval :: Env -> Exp -> Value
eval env = \case
  ...
  App l r -> apply (eval env l) (eval env r)
  Lam x e -> undefined -- TODO (hole1)

apply :: Value -> Value -> Value
apply = undefined -- TODO (hole2)
```

We also extended the Value type with a placeholder for some new kind of _functional values_ which we think we will need.

```
data Value
  = VI Int
  | VS String
  | VF -- TODO: (hole3)
```

Now you must fill these three holes!


## Tips

What is a functional value? -- A value of form `(VF ...)`

What does it represent?... it needs to represent somehow a delayed evaluation.... evaluation waiting for an argument value before it can proceed.

Where does it get created?... one place... when we reach a Lambda expression.

What can we do with it?... one thing... `apply` it!

Note that `apply` will expect it's first argument to be a functional-value (makes no sense to apply a number!), but the second argument can be any kind of value. Code something like:

```
apply :: Value -> Value -> Value
apply func arg = case func of
  VF ... ->
    undefined -- TODO
  _ ->
    error "apply: arg1 not a function!"
```

How should `apply` restart evaluation? ... a call to `eval` perhaps... ?
What things will it need in hand?
This will help you figure what `VF` needs to carry.
And of course `VF` can carry any item which is in scope when it is created.

Ok. enough cryptic clues!
Good luck
Dad xxx
