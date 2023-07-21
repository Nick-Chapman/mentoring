
#include "stdio.h"

int fact (int n) {
  if (n==0) {
    return 1;
  } else {
    return n * fact(n-1);
  }
}

int fib (int n) {
  if (n<2) {
    return n;
  } else {
    return fib(n-1) + fib(n-2);
  }
}

int main() {
  printf("cps\n");
  printf("fact(6)=%d\n",fact(6));
  printf("fib(10)=%d\n",fib(10));
  return 0;
}
