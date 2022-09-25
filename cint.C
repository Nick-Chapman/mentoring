
#include <stdio.h>
#include <string>

#include "lexer.h"

void test_exp_ast(void);
void test_prog_ast(void);
void test_lexer(void);

int main(int argc, char* argv[]) {

  bool run_test_exp_ast = false;
  bool run_test_prog_ast = false;
  bool run_test_lexer = false;

  for (int i = 1; i < argc; i++) {
    std::string arg = argv[i];
    if (arg == "-test-exp") run_test_exp_ast = true;
    if (arg == "-test-prog") run_test_prog_ast = true;
    if (arg == "-test-lex") run_test_lexer = true;
    if (arg == "-test") {
      run_test_exp_ast = true;
      run_test_prog_ast = true;
      run_test_lexer = true;
    }
  }
  if (run_test_exp_ast) test_exp_ast();
  if (run_test_prog_ast) test_prog_ast();
  if (run_test_lexer) test_lexer();
  return 0;
}

void test_lexer(void) {

  // TODO: read program string from file
  LexState ls =
    LexState("def fib(x):\n if (x < 2) then 1 else fib(x-1) + fib(x-2) ");

  int i = 0;
  Token tok = ls.get_token();
  while (tok.kind() != NoMoreTokens) {
    printf("%2d) %02d-%02d '%s' %s \n", i,
           tok.startPos(), tok.endPos(), tok.text().c_str(), showKind(tok.kind()).c_str());
    i++;
    tok = ls.get_token();
  }
}


// interface types
typedef std::string identifier; //TODO: rename -> "name"
class Exp;
typedef Exp* exp;
class Value;
typedef Value* value;
class Env;
typedef Env* env;

// interface...

// expression abstract syntax, and proposed future concrete syntax:
exp num(int);           // 42
exp add(exp,exp);       // A + B
exp mul(exp,exp);       // A * B
exp sub(exp,exp);       // A - B
exp less(exp,exp);      // A < B
exp ite(exp,exp,exp);   // (1) if A then B else C  *or*  (2) A ? B : C
exp var(identifier);    // x
exp str(std::string);   // "foo"
exp append(exp,exp);    // A ++ B
exp let(identifier,exp,exp); // let X = RHS in BODY
exp call1(identifier f,exp arg); // f(arg)

// interface: values
value vInt(int);
value vString(std::string);
std::string showV(value);
bool equalV(value,value);

// evaluation; pretty-printing
value eval(exp,env);
std::string show(exp);


// interface: definitions and programs

class Program;
typedef Program* program;
class Defs;
typedef Defs* defs;
class Def;
typedef Def* def;

program makeProgram(defs, exp main);
defs nilDefs();
defs consDefs(def,defs);
def def1(identifier name, identifier arg, exp body);
value execute(program);


void crash(std::string mes) {
  printf("CRASH: %s\n", mes.c_str());
  fflush(stdout);
  abort();
}

// testing...
void t2(exp example, env env, value expected) {
  printf("[%s] -> ", show(example).c_str());
  value actual = eval(example,env);
  bool pass = equalV(actual,expected);
  if (pass) {
    printf("%s\n", showV(actual).c_str());
  } else {
    printf("%s [expect:%s] FAIL\n", showV(actual).c_str(), showV(expected).c_str());
  }
  if (!pass) crash("test failed: " + show(example));
}

// interface: environments...
env emptyEnv();
env extendEnv(env,identifier,value);
value lookupEnv(env,identifier);

void t(exp example, value expected) {
  t2(example, emptyEnv(), expected);
}

void test_exp_ast(void) {
  t( num(42), vInt(42));
  t( add(num(42),num(3)), vInt(45) );
  //t( add(num(11),num(22)), vInt(77) ); //expect fail
  t( mul(num(42),num(3)), vInt(126) );
  t( sub(num(42),num(3)), vInt(39) );
  t( sub(num(42),sub(num(10),num(3))), vInt(35) );
  t( sub(sub(num(42),num(10)),num(3)), vInt(29) );
  t( less(num(5),num(5)), vInt(0) );
  t( less(num(5),num(6)), vInt(1) );
  t( ite (num(1), num(100), num(200)), vInt(100) );
  t( ite (num(0), num(100), num(200)), vInt(200) );

  env env0 = extendEnv(emptyEnv(), "x", vInt(4));
  env env1 = extendEnv(env0, "y", vInt(7));
  env env2 = extendEnv(env0, "y", vInt(8));
  env env3 = extendEnv(env1, "x", vInt(5));

  exp xyExpression = mul(var("x"),var("y"));
  t2( xyExpression, env1, vInt(28) );
  t2( xyExpression, env2, vInt(32) );
  t2( xyExpression, env3, vInt(35) );

  t( str("foo"), vString("foo") );
  t( append(str("foo"),str("bar")), vString("foobar") );

  t ( let("x", add(num(39),num(3)), add(num(100),var("x"))), vInt(142) );
  t ( let ("x", num(3), let("x", mul(var("x"),var("x")), mul(var("x"),var("x")))), vInt(81) );
}


defs allDefs() {
/*
  def square(x): x*x

  def fact(x):
    if x < 1:
      1
    else:
      x * fact (n-1)

  def fib(x):
    if (x < 2) then 1 else fib(n-1) + fib(n-2)
*/

  def squareDef =
    def1("square","x",
         mul(var("x"),var("x")));
  def factorialDef =
    def1("fact","x",
         ite (less(var("x"),num(1)),
              num(1),
              mul(var("x"),call1("fact",sub(var("x"),num(1))))));
  def fibDef =
    def1("fib","x",
         ite (less(var("x"),num(2)),
              num(1),
              add(call1("fib",sub(var("x"),num(1))),
                  call1("fib",sub(var("x"),num(2))))));
  return
    consDefs(factorialDef,
    consDefs(squareDef,
    consDefs(fibDef,
    nilDefs())));
}

void tp(exp mainExp) {
  defs defs = allDefs();
  program theProg = makeProgram(defs,mainExp);
  value res = execute(theProg);
  printf("%s --> %s\n", show(mainExp).c_str(), showV(res).c_str());
}

void test_prog_ast(void) {
  printf("**testPrograms...\n");
  tp(call1("fact",num(5)));
  tp(call1("fact",call1("fact",num(3))));
  tp(call1("square",call1("fact",num(3))));
  tp(call1("fact",call1("square",num(3))));
  tp(call1("fib",num(10)));
  tp(call1("fib",num(20)));
}


// implementation: environments...

class Env {
public:
  virtual value lookup(identifier) = 0;
};

class EmptyEnv : public Env {
public:
  value lookup(identifier name) {
    printf("CRASH: EmptyEnv.lookup(%s)\n", name.c_str());
    abort();
    return 0;
  }
};

class ExtendedEnv : public Env {
  env _env;
  identifier _name;
  value _value;
public:
  ExtendedEnv(env env,identifier name,value value) : _env(env), _name(name), _value(value) {}
  value lookup(identifier sought) {
    if (_name == sought) {
      return _value;
    } else {
      return _env->lookup(sought);
    }
  }
};

env emptyEnv() {
  return new EmptyEnv();
}

env extendEnv(env env,identifier name,value value) {
  return new ExtendedEnv(env,name,value);
}
value lookupEnv(env env,identifier name) {
  return env->lookup(name);
}


// implementation: values...

class Value {
public:
  virtual int getInt() = 0;
  virtual std::string getString() = 0;
  virtual std::string show() = 0;
  virtual bool isEqualTo(value) = 0;
};

class ValueInt : public Value {
  int _n;
public:
  ValueInt(int n) :_n(n) {}
  int getInt() {
    return _n;
  }
  std::string getString() {
    crash("ValueInt::getString()");
    return "oops!";
  }
  std::string show() {
    return std::to_string(_n);
  }
  bool isEqualTo(value v2) {
    return _n == v2->getInt();
  }
};

class ValueString : public Value {
  std::string _s;
public:
  ValueString(std::string s) :_s(s) {}
  int getInt() {
    crash("ValueString::getInt()");
    return 0;
  }
  std::string getString() {
    return _s;
  }
  std::string show() {
    return std::string("\"") + _s + std::string("\"");
  }
  bool isEqualTo(value v2) {
    return _s == v2->getString();
  }
};


// interface: values
value vInt(int n) {
  return new ValueInt(n);
}

value vString(std::string s) {
  return new ValueString(s);
}

std::string showV(value value) {
  return value->show();
}

bool equalV(value v1,value v2) {
  return v1->isEqualTo(v2);
}

// implementation: expressions...

class Exp {
public:
  virtual value eval(defs,env) = 0;
  virtual std::string show() = 0;
};

value eval(exp exp, env env) {
  return exp->eval(nilDefs(), env);
}

std::string show(exp e) {
  return e->show();
}

class Num : public Exp {
  int _n;
public:
  Num(int n) : _n(n) {}
  value eval(defs defs, env env) {
    return vInt(_n);
  }
  std::string show() {
    return std::to_string(_n);
  }
};

class String : public Exp {
  std::string _s;
public:
  String(std::string s) : _s(s) {}
  value eval(defs defs, env env) {
    return vString(_s);
  }
  std::string show() {
    return std::string("\"") + _s + std::string("\"");
  }
};

class Add : public Exp {
  exp _x;
  exp _y;
public:
  Add(exp x, exp y) : _x(x), _y(y) {}
  value eval(defs defs, env env) {
    return vInt(_x->eval(defs,env)->getInt() + _y->eval(defs,env)->getInt());
  }
  std::string show() {
    return std::string("(") + _x->show() + " + " + _y->show() + ")";
  }
};

class Mul : public Exp {
  exp _x;
  exp _y;
public:
  Mul(exp x, exp y) : _x(x), _y(y) {}
  value eval(defs defs, env env) {
    return vInt(_x->eval(defs,env)->getInt() * _y->eval(defs,env)->getInt());
  }
  std::string show() {
    return std::string("(") + _x->show() + " * " + _y->show() + ")";
  }
};

class Sub : public Exp {
  exp _x;
  exp _y;
public:
  Sub(exp x, exp y) : _x(x), _y(y) {}
  value eval(defs defs, env env) {
    return vInt(_x->eval(defs,env)->getInt() - _y->eval(defs,env)->getInt());
  }
  std::string show() {
    return std::string("(") + _x->show() + " - " + _y->show() + ")";
  }
};

class LessThan : public Exp {
  exp _x;
  exp _y;
public:
  LessThan(exp x, exp y) : _x(x), _y(y) {}
  value eval(defs defs, env env) {
    return vInt(_x->eval(defs,env)->getInt() < _y->eval(defs,env)->getInt() ? 1 : 0);
  }
  std::string show() {
    return std::string("(") + _x->show() + " < " + _y->show() + ")";
  }
};

class IfThenElse : public Exp {
  exp _i;
  exp _t;
  exp _e;
public:
  IfThenElse(exp i, exp t, exp e) : _i(i), _t(t), _e(e) {}
  value eval(defs defs, env env) {
    if (_i->eval(defs,env)->getInt() == 1) {
      return _t->eval(defs,env);
    } else {
      return _e->eval(defs,env);
    }
  }
  std::string show() {
    return "(if " + _i->show() + " then " + _t->show() + " else " + _e->show() + ")";
  }
};

class LetExpression : public Exp {
  identifier _x;
  exp _rhs;
  exp _body;
public:
  LetExpression(identifier x, exp rhs, exp body) : _x(x), _rhs(rhs), _body(body) {}
  value eval(defs defs, env env0) {
    value rhsValue = _rhs->eval(defs,env0);
    env env1 = extendEnv(env0, _x, rhsValue);
    return _body->eval(defs,env1);
  }
  std::string show() {
    return "let " + _x + " = " + _rhs->show() + " in " + _body->show();
  }
};

class Variable : public Exp {
  identifier _name;
public:
  Variable(identifier name) : _name(name) {}
  value eval(defs defs, env env) {
    return lookupEnv(env,_name);
  }
  std::string show() {
    return _name;
  }
};

class Append : public Exp {
  exp _x;
  exp _y;
public:
  Append(exp x, exp y) : _x(x), _y(y) {}
  value eval(defs defs, env env) {
    return vString(_x->eval(defs,env)->getString() + _y->eval(defs,env)->getString());
  }
  std::string show() {
    return std::string("(") + _x->show() + " ++ " + _y->show() + ")";
  }
};


exp num(int n) { return new Num(n); }
exp str(std::string s) { return new String(s); }
exp add(exp x ,exp y) { return new Add(x,y); }
exp mul(exp x ,exp y) { return new Mul(x,y); }
exp sub(exp x ,exp y) { return new Sub(x,y); }
exp less(exp x ,exp y) { return new LessThan(x,y); }
exp ite(exp i ,exp t, exp e) { return new IfThenElse(i,t,e); }
exp var(identifier name) { return new Variable(name); }
exp append(exp a,exp b) { return new Append(a,b); }
exp let(identifier x,exp r,exp b) { return new LetExpression(x,r,b); }


// implementation: definitions and programs

class Defs {
public:
  virtual def findDef(identifier) = 0;
};

class Def {
public:
  virtual identifier getName() = 0;
  virtual value apply(defs,value) = 0;
};

class NilDefs : public Defs {
public:
  NilDefs() {}
  def findDef(identifier name) {
    printf("CRASH: NilDefs.findDef(%s)\n", name.c_str());
    abort();
    return 0;
  }
};

class ConsDefs : public Defs {
  def _first;
  defs _more;
public:
  ConsDefs(def d1,defs ds) : _first(d1), _more(ds) {}
  def findDef(identifier sought) {
    if (_first->getName() == sought) {
      return _first;
    } else {
      return _more->findDef(sought);
    }
  }
};

class Call1 : public Exp {
  identifier _func;
  exp _arg;
public:
  Call1(identifier f, exp a) : _func(f), _arg(a) {}
  value eval(defs defs, env env) {
    def called = defs->findDef(_func);
    value v = _arg->eval(defs,env);
    return called->apply(defs,v);
  }
  std::string show() {
    return _func + "(" + _arg->show() + ")";
  }
};

class Def1 : public Def {
private:
  identifier _name;
  identifier _formal;
  exp _body;
public:
  Def1(identifier name, identifier formal, exp body)
    : _name(name), _formal(formal), _body(body) {}
  identifier getName() { return _name; }
  value apply(defs defs, value actual) {
    //printf("Def1.apply\n");
    env env = extendEnv(emptyEnv(),_formal,actual);
    return _body->eval(defs,env);
  }
};

class Program {
  defs _theDefs;
  exp _mainExp;
public:
  Program(defs ds, exp e) : _theDefs(ds), _mainExp(e) {}
  value execute() {
    return _mainExp->eval(_theDefs,emptyEnv());
  }
};


exp call1(identifier f,exp arg) {
  return new Call1(f,arg);
}
def def1(identifier name, identifier arg, exp body) {
  return new Def1(name,arg,body);
}
defs nilDefs() {
  return new NilDefs();
}
defs consDefs(def d1,defs ds) {
  return new ConsDefs(d1,ds);
}
program makeProgram(defs ds, exp main) {
  return new Program(ds,main);
}
value execute(program p) {
  return p->execute();
}
