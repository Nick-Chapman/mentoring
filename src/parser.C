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
        + tok.startRC()
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

exp parse_expression(LexState& ls);

exp parse_exp4(LexState& ls) {
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
  case LP: {
    exp e = parse_expression(ls);
    ensure_token(ls,RP);
    return e;
  }
  default: {
    error(tok,"<Exp>");
  }
  }
  return 0;
}


exp parse_exp3(LexState& ls) {
  exp l = parse_exp4(ls);
  if (peek_token(ls) == Star) {
    ensure_token(ls,Star);
    exp r = parse_exp4(ls);
    return mul(l,r);
  }
  return l;
}

exp parse_exp2(LexState& ls) {
  exp l = parse_exp3(ls);
  if (peek_token(ls) == Langle) {
    ensure_token(ls,Langle);
    exp r = parse_exp3(ls);
    return less(l,r);
  }
  return l;
}

exp parse_exp1(LexState& ls) {
  exp l = parse_exp2(ls);
  if (peek_token(ls) == Plus) {
    ensure_token(ls,Plus);
    exp r = parse_exp2(ls);
    return add(l,r);
  }
  if (peek_token(ls) == Dash) {
    ensure_token(ls,Dash);
    exp r = parse_exp2(ls);
    return sub(l,r);
  }
  return l;
}

exp parse_expression(LexState& ls) {

  if (peek_token(ls) == If) {
    ensure_token(ls,If);
    exp i = parse_expression(ls);
    ensure_token(ls,Then);
    exp t = parse_expression(ls);
    ensure_token(ls,Else);
    exp e = parse_expression(ls);
    return ite(i,t,e);
  }
  //if (peek_token(ls) == Let) {
  return parse_exp1(ls);
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
