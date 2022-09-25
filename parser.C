#include <string>
#include "parser.h"
#include "ast.h"

static void crash(std::string mes) {
  printf("CRASH: %s\n", mes.c_str());
  fflush(stdout);
  abort();
}

void error(Token tok, std::string mes) {
  crash(mes + "; got '" + tok.text() + "' ("
        + showKind(tok.kind()) + ") at "
        + std::to_string(tok.startPos()) + ":"
        + std::to_string(tok.endPos())
        );
}

void ensure_token(LexState& ls,Kind kind) {
  Token tok = ls.get_token();
  if (tok.kind() != kind) {
    error(tok,"expected: " + showKind(kind));
  }
}

Kind peek_token(LexState ls) { // **no '&'; state unchanged
  Token tok = ls.get_token();
  return tok.kind();
}

exp parse_expression(LexState& ls) {
  Token tok = ls.get_token();
  switch (tok.kind()) {
  case Identifier: {
    if (peek_token(ls) == LP) {
      ensure_token(ls,LP);
      exp actual = parse_expression(ls);
      ensure_token(ls,RP);
      return call1(tok.text(),actual);
    } else {
      return var(tok.text());
    }
  }
  case Number: {
    int n;
    sscanf(tok.text().c_str(),"%d",&n);
    return num(n);
  }
  case If: {
    exp i = parse_expression(ls);
    ensure_token(ls,Then);
    exp t = parse_expression(ls);
    ensure_token(ls,Else);
    exp e = parse_expression(ls);
    return ite(i,t,e);
  }
  case LP: {
    exp l = parse_expression(ls);
    Token tok = ls.get_token();
    switch (tok.kind()) {
    case Plus: {
      exp r = parse_expression(ls);
      ensure_token(ls,RP);
      return add(l,r);
    }
    case Dash: {
      exp r = parse_expression(ls);
      ensure_token(ls,RP);
      return sub(l,r);
    }
    case Star: {
      exp r = parse_expression(ls);
      ensure_token(ls,RP);
      return mul(l,r);
    }
    case Langle: {
      exp r = parse_expression(ls);
      ensure_token(ls,RP);
      return less(l,r);
    }
    default: {
      error(tok,"<Exp> expected binary-operator");
    }
    }
  }
  default: {
    error(tok,"<Exp>");
  }
  }
  return 0;
}

name parse_name(LexState& ls) {
  Token tok = ls.get_token();
  switch (tok.kind()) {
    case Identifier: {
      return tok.text();
    }
  default: {
    error(tok,"<Name>");
  }
  }
  return 0;
}

def parse_def(LexState& ls) {
  Token tok = ls.get_token();
  switch (tok.kind()) {
  case DefKeyword: {
    name func = parse_name(ls);
    ensure_token(ls,LP);
    name arg = parse_name(ls);
    ensure_token(ls,RP);
    ensure_token(ls,Colon);
    exp body = parse_expression(ls);
    ensure_token(ls,Semi);
    return def1(func,arg,body);
  }
  default: {
    error(tok,"<Def>");
  }
  }
  return 0;
}

defs parse_defs(LexState& ls) {
  if (peek_token(ls) == DefKeyword) {
    def d1 = parse_def(ls);
    defs ds = parse_defs(ls);
    return consDefs(d1,ds);
  } else {
    return nilDefs();
  }
}

program parse_program(LexState& ls) {
  defs ds = parse_defs(ls);
  exp main = parse_expression(ls);
  return makeProgram(ds,main);
}
