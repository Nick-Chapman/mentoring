
Hey Ellie,

Your homework, should you choose to accept it, is to extend your burgeoning interpreter to support non-integer values. And then support expression-forms for string-literals and a concatenation operator.

Start by changing the type of `Value`, from:
```
  type Value = Int
```
to something like:
```
  data Value = VI Int | VS String
```
then fix up everything. This is most of the effort!...

Tip: The eval code for the Add case currently looks something like:
```
  Add l r -> eval env l + eval env r
```
Change it to:
```
  Add l r -> addV (eval env l) (eval env r)
```
pushing the guts of the work to a new top level definition, called addV (for "add Values")
```
  addV :: Value -> Value -> Value
  addV = undefined -- TODO: implement me!
```
I cant remember if you already added any other binary ops for subtraction/multiplication.
If so, they will need similar treatment.

To implement them, you will find a helper like this, helpful (!)
```
  getI :: Value -> Int
  getI = undefined
```
When eventhing is working again.
You need to add two new constructors to your `Exp` type:
```
data Exp
  = Lit Int
  | LitS String -- I AM NEW
  | Var Identifier
  | Let Identifier Exp Exp
  | Add Exp Exp
  | Subtract Exp Exp
  | Multiply Exp Exp
  | Concat Exp Exp  -- I AM NEW
```
Support the two new cases in `show` and `eval`.

Drive with an example like this:
```
  Let "hw" (Concat (LitS "hello, ") (LitS "world")) (Concat (Var "hw") (LitS "!!"))
```

Have fun,
Dad xxx
