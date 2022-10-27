
start = program

expression =
  | "if" expression "then" expression "else" expression
  | "let" Identifier '=' expression "in" expression
  | expression '<' expression
  | expression '>' expression
  | expression '+' expression
  | expression '-' expression
  | expression '*' expression
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
