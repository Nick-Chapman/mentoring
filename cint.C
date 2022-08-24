
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

// abstract syntax, and proposed future concrete syntax:
exp num(int);           // 42
exp add(exp,exp);       // A + B
exp mul(exp,exp);       // A * B
exp sub(exp,exp);       // A - B
exp less(exp,exp);      // A < B
exp ite(exp,exp,exp);   // (1) if A then B else C  *or*  (2) A ? B : C
exp var(identifier);    // x

// evaluation; pretty-printing
value eval(exp);
std::string show(exp);

// testing...
void t(exp example, int expected) {
  printf("[%s] -> ", show(example).c_str());
  int actual = eval(example);
  bool pass = actual == expected;
  if (pass) {
    printf("%d\n", actual);
  } else {
    printf("%d [expect:%d] FAIL\n", actual, expected);
  }
  //if (!pass) std::abort();
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
  t( mul(var("x"),var("y")), 28 ); //x=4,y=7
}

// implementation...

class Exp {
public:
  virtual value eval() = 0;
  virtual std::string show() = 0;
};

value eval(exp e) {
  return e->eval();
}

std::string show(exp e) {
  return e->show();
}

class Num : public Exp {
  int _n;
public:
  Num(int n) : _n(n) {}
  value eval() {
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
  value eval() {
    return _x->eval() + _y->eval();
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
  value eval() {
    return _x->eval() * _y->eval();
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
  value eval() {
    return _x->eval() - _y->eval();
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
  value eval() {
    return _x->eval() < _y->eval() ? 1 : 0;
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
  value eval() {
    if (_i->eval() == 1) {
      return _t->eval();
    } else {
      return _e->eval();
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
  value eval() {
    if (_name == "x") {
      return 7;
    }
    else if (_name == "y") {
      return 4;
    }
    else {
      abort();
    }
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
