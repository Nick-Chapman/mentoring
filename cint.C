#include <stdio.h>
#include <string>
#include <fstream>
#include <sstream>
#include "ast.h"
#include "lexer.h"

static void test_exp_ast(void);
static void test_prog_ast(void);
static void test_lex_file(std::string path);

static void crash(std::string mes) {
  printf("cint/CRASH: %s\n", mes.c_str());
  fflush(stdout);
  abort();
}

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
  if (run_test_lexer) test_lex_file("examples/fact.prog");

  return 0;
}

void test_lex_string(std::string str) {
  LexState ls = LexState(str);
  int i = 0;
  Token tok = ls.get_token();
  while (tok.kind() != NoMoreTokens) {
    printf("%2d) %02d-%02d '%s' %s \n", i,
           tok.startPos(), tok.endPos(), tok.text().c_str(), showKind(tok.kind()).c_str());
    i++;
    tok = ls.get_token();
  }
}

void test_lex_file(std::string path) {
  printf("test_lex_file: %s\n", path.c_str());
  std::ifstream ifs(path); // TODO: error when path does not exist
  std::stringstream buffer;
  buffer << ifs.rdbuf();
  std::string contents = buffer.str();
  test_lex_string(contents);
}

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
