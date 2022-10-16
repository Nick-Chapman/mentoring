#include <stdio.h>
#include "lexer.h"

int main(int argc, char* argv[]) {
  printf("play lex and yacc..\n");
  lex_entry("the quick 42 fox\nblah");
  return 0;
}
