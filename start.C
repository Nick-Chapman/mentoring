
#include <stdio.h>

void test(void);

int main() {
  printf("**c-interpreter\n");
  test();
  return 0;
}

// interface...
class XXX;
typedef XXX* exp;
typedef int value;
value eval(exp);
exp num(int);
exp add(exp,exp);

// testing...
void test(void) {
  exp example = add(num(42),num(3));
  int result = eval(example);
  printf("result: %d\n", result);
}
