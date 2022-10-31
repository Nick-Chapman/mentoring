#include "ast.h"

static void crash(std::string mes) {
  printf("ast/CRASH: %s\n", mes.c_str());
  fflush(stdout);
  abort();
}

// implementation: environments...

class Env {
public:
  virtual value lookup(name) = 0;
};

class EmptyEnv : public Env {
public:
  value lookup(name name) {
    printf("CRASH: EmptyEnv.lookup(%s)\n", name.c_str());
    abort();
    return 0;
  }
};

class ExtendedEnv : public Env {
  env _env;
  name _name;
  value _value;
public:
  ExtendedEnv(env env,name name,value value) : _env(env), _name(name), _value(value) {}
  value lookup(name sought) {
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

env extendEnv(env env,name name,value value) {
  return new ExtendedEnv(env,name,value);
}
value lookupEnv(env env,name name) {
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
  name _x;
  exp _rhs;
  exp _body;
public:
  LetExpression(name x, exp rhs, exp body) : _x(x), _rhs(rhs), _body(body) {}
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
  name _name;
public:
  Variable(name name) : _name(name) {}
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
exp var(name name) { return new Variable(name); }
exp append(exp a,exp b) { return new Append(a,b); }
exp let(name x,exp r,exp b) { return new LetExpression(x,r,b); }


// implementation: definitions and programs

class Defs {
public:
  virtual std::string show() = 0;
  virtual def findDef(name) = 0;
};

class Def {
public:
  virtual std::string show() = 0;
  virtual name getName() = 0;
  virtual value apply(defs,value) = 0;
};

class NilDefs : public Defs {
public:
  NilDefs() {}
  std::string show() { return ""; }
  def findDef(name name) {
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
  std::string show() {
    return _first->show() + _more->show();
  }
  def findDef(name sought) {
    if (_first->getName() == sought) {
      return _first;
    } else {
      return _more->findDef(sought);
    }
  }
};

class Call1 : public Exp {
  name _func;
  exp _arg;
public:
  Call1(name f, exp a) : _func(f), _arg(a) {}
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
  name _func;
  name _formal;
  exp _body;
public:
  Def1(name func, name formal, exp body)
    : _func(func), _formal(formal), _body(body) {}
  std::string show() {
    return "def " +_func + "(" + _formal + "):\n  " + _body->show() + ";\n\n";
  }
  name getName() { return _func; }
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
  std::string show() {
    return _theDefs->show() + _mainExp->show();
  }
};


exp call1(name f,exp arg) {
  return new Call1(f,arg);
}
def def1(name func, name arg, exp body) {
  return new Def1(func,arg,body);
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
std::string showProgram(program p) {
  return p->show();
}
