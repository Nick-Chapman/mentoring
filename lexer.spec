
lex-ish specification for our lexer...

Whitespace: [ \t\n]+

Symbols:
"-"
"+"
"*"
"**"
"("
")"
"<"
">"
":"

Keywords:
"if"
"then"
"else"
"def"

Identifier: [a-zA-Z_][a-zA-Z0-9_]*
Number: [0-9]+
