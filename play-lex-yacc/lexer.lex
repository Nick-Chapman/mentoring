%{
#include "lexer.h"
  enum Kind {Number = 100,Identifier,Unknown};
  //  enum Kind g_kind;
%}
%%
[ \t] ;
[0-9]+ {
  //printf("number<%s>\n", yytext);
  return Number;
}
[a-zA-Z]+ {
  //printf("identifier<%s>\n", yytext);
  return Identifier;
}
. {
  printf("unknown<%s>\n", yytext);
  return Unknown;
}
\n {
  //printf("newline\n");
  return Unknown;
}
%%

void lex_entry(char* string) {
  yy_scan_string(string);
  int r;
  while ((r = yylex())) {
    printf("r=%d\n",r);
  }
}

int yywrap() {
  return 1; // NICK ?
}

void lala() {
  input();
  yyunput(1,0);
}
