
start = program

expression = // general-expression
  | "if" expression "then" expression "else" expression
  | "let" Identifier '=' expression "in" expression
  | exp1

exp1 = // relational-expression; less/greater-than; not-associative
  | exp2 '<' exp2
  | exp2 '>' exp2
  | exp2

exp2 = // term-expression: add/minus; left-associative
  | exp3 exp2x

exp2x = // term-expression (continuation)
  | '+' exp3 exp2x
  | '-' exp3 exp2x
  | <nothing>

exp3 = // product-expression: multiplication; left-associative
  | exp4 exp3x

exp3x = // product-expression (continuation)
  | '*' exp4 exp3x
  | <nothing>

exp4 = // atomic-expression
  | Number
  | Identifier
  | Identifier '(' expression ')
  | '(' expression ')'

definition =
  | "def" Identifier '(' Identifier ')' ':' expression ';'

definition_list =
  | <nothing>
  | definition definition_list

program =
  | definition_list expression
