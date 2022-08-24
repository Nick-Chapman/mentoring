
#include <stdio.h>
#include <string>

static void test(void);

int main() {
  printf("**c-interpreter\n");
  test();
  return 0;
}

// interface types
typedef std::string identifier;
class Exp;
typedef Exp* exp;
class Value;
typedef Value* value;
class Env;
typedef Env* env;

// interface: expression abstract syntax, and proposed future concrete syntax:
exp num(int);           // 42
exp add(exp,exp);       // A + B
exp mul(exp,exp);       // A * B
exp sub(exp,exp);       // A - B
exp less(exp,exp);      // A < B
exp ite(exp,exp,exp);   // (1) if A then B else C  *or*  (2) A ? B : C
exp var(identifier);    // x

exp str(std::string);   // "foo"
exp append(exp,exp);    // A ++ B

// interface: values
value vInt(int);
std::string showV(value);
bool equalV(value,value);

// evaluation; pretty-printing
value eval(exp,env);
std::string show(exp);

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
  //if (!pass) std::abort();
}

// interface: environments...
env emptyEnv();
env extendEnv(env,identifier,value);
value lookupEnv(env,identifier);

void t(exp example, value expected) {
  t2(example, emptyEnv(), expected);
}

void test(void) {
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
}


void crash(std::string mes) {
  printf("CRASH: %s\n", mes.c_str());
  fflush(stdout);
  std::abort();
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
    crash("EmptyEnv.lookup");
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
  std::string show() {
    return std::to_string(_n);
  }
  bool isEqualTo(value v2) {
    return _n == v2->getInt();
  }
};

// interface: values
value vInt(int n) {
  return new ValueInt(n);
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
  virtual value eval(env) = 0;
  virtual std::string show() = 0;
};

value eval(exp exp, env env) {
  return exp->eval(env);
}

std::string show(exp e) {
  return e->show();
}

class Num : public Exp {
  int _n;
public:
  Num(int n) : _n(n) {}
  value eval(env env) {
    return vInt(_n);
  }
  std::string show() {
    return std::to_string(_n);
  }
};

class Add : public Exp {
  exp _x;
  exp _y;
public:
  Add(exp x, exp y) : _x(x), _y(y) {}
  value eval(env env) {
    return vInt(_x->eval(env)->getInt() + _y->eval(env)->getInt());
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
  value eval(env env) {
    return vInt(_x->eval(env)->getInt() * _y->eval(env)->getInt());
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
  value eval(env env) {
    return vInt(_x->eval(env)->getInt() - _y->eval(env)->getInt());
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
  value eval(env env) {
    return vInt(_x->eval(env)->getInt() < _y->eval(env)->getInt() ? 1 : 0);
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
  value eval(env env) {
    if (_i->eval(env)->getInt() == 1) {
      return _t->eval(env);
    } else {
      return _e->eval(env);
    }
  }
  std::string show() {
    return std::string("(if ") + _i->show() + " then " + _t->show() + " else " + _e->show() + ")";
  }
};

class Variable : public Exp {
  identifier _name;
public:
  Variable(identifier name) : _name(name) {}
  value eval(env env) {
    return lookupEnv(env,_name);
  }
  std::string show() {
    return _name;
  }
};


exp num(int n) { return new Num(n); }
exp add(exp x ,exp y) { return new Add(x,y); }
exp mul(exp x ,exp y) { return new Mul(x,y); }
exp sub(exp x ,exp y) { return new Sub(x,y); }
exp less(exp x ,exp y) { return new LessThan(x,y); }
exp ite(exp i ,exp t, exp e) { return new IfThenElse(i,t,e); }
exp var(identifier name) { return new Variable(name); }
