
Exp =
  | Identifier
  | Identifier '(' Exp ')'
  | Number
  | 'if' Exp 'then' Exp 'else' Exp
  | '(' Exp BinOp Exp ')'

BinOp = '+' | '-' | '*' | '<'

Def =
  | 'def' Identifier '(' Identifier ')' ':' Exp ';'

DefList =
  | <nothing>
  | Def DefList

Program =
  | DefList Exp
