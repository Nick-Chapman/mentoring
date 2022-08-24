
#include <stdio.h>
#include <string>

static void test(void);

int main() {
  printf("**c-interpreter\n");
  test();
  return 0;
}

// interface...
class Exp;
typedef Exp* exp;
typedef int value;
typedef std::string identifier;

class Env;
typedef Env* env;

// abstract syntax, and proposed future concrete syntax:
exp num(int);           // 42
exp add(exp,exp);       // A + B
exp mul(exp,exp);       // A * B
exp sub(exp,exp);       // A - B
exp less(exp,exp);      // A < B
exp ite(exp,exp,exp);   // (1) if A then B else C  *or*  (2) A ? B : C
exp var(identifier);    // x

// evaluation; pretty-printing
value eval(exp,env);
std::string show(exp);

// testing...
void t2(exp example, env env, int expected) {
  printf("[%s] -> ", show(example).c_str());
  int actual = eval(example,env);
  bool pass = actual == expected;
  if (pass) {
    printf("%d\n", actual);
  } else {
    printf("%d [expect:%d] FAIL\n", actual, expected);
  }
  //if (!pass) std::abort();
}

// interface: environments...
env emptyEnv();
env extendEnv(env,identifier,value);
value lookupEnv(env,identifier);

void t(exp example, int expected) {
  t2(example, emptyEnv(), expected);
}

void test(void) {
  t( num(42), 42);
  t( add(num(42),num(3)), 45 );
  t( mul(num(42),num(3)), 126 );
  t( sub(num(42),num(3)), 39 );
  t( sub(num(42),sub(num(10),num(3))), 35 );
  t( sub(sub(num(42),num(10)),num(3)), 29 );
  t( less(num(5),num(5)), 0 );
  t( less(num(5),num(6)), 1 );
  t( ite (num(1), num(100), num(200)), 100 );
  t( ite (num(0), num(100), num(200)), 200 );

  env env0 = extendEnv(emptyEnv(), "x", 4);
  env env1 = extendEnv(env0, "y", 7);
  env env2 = extendEnv(env0, "y", 8);
  env env3 = extendEnv(env1, "x", 5);

  exp xyExpression = mul(var("x"),var("y"));
  t2( xyExpression, env1, 28 );
  t2( xyExpression, env2, 32 );
  t2( xyExpression, env3, 35 );
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
    return _n;
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
    return _x->eval(env) + _y->eval(env);
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
    return _x->eval(env) * _y->eval(env);
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
    return _x->eval(env) - _y->eval(env);
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
    return _x->eval(env) < _y->eval(env) ? 1 : 0;
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
    if (_i->eval(env) == 1) {
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
