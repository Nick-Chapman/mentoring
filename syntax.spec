
start = program

expression =
  | Identifier
  | Number
//  | '(' expression ')'
//  | expression bin_op expression
  | "if" expression "then" expression "else" expression
  | Identifier '(' expression ')
  | '(' expression bin_op expression ')'

bin_op = '+' | '-' | '*' | '<'

definition =
  | "def" Identifier '(' Identifier ')' ':' expression ';'

definition_list =
  | <nothing>
  | definition definition_list

program =
  | definition_list expression
