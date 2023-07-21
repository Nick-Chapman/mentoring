
#include "stdio.h"

int fact (int n) {
  if (n==0) {
    return 1;
  } else {
    return n * fact(n-1);
  }
}

/*int fib (int n) {
  if (n<2) {
    return n;
  } else {
    return fib(n-1) + fib(n-2);
  }
}*/

class FK {
public:
  virtual int apply(int) =0;
};

int fibD (int n, FK* k);

class KDone : public FK {
public:
  int apply(int res) {
    return res;
  }
};

struct KTwo : public FK {
  FK* k;
  int res1;
  int apply(int res2) {
    return k->apply(res1+res2);
  }
};

struct KOne : public FK {
  int n;
  FK* k;
  int apply(int res1) {
    KTwo* k2 = new KTwo;
    k2->res1 = res1;
    k2->k = k;
    return fibD(n-2, k2);
  }
};

int fibD (int n, FK* k) {
  if (n < 2) {
    return k->apply(n);
  } else {
    KOne* k1 = new KOne;
    k1->n = n;
    k1->k = k;
    return fibD(n-1, k1);
  }
}

int fib (int n) {
  return fibD(n, new KDone);
}

int main() {
  printf("cps\n");
  printf("fact(6)=%d\n",fact(6));
  printf("fib(10)=%d\n",fib(10));
  return 0;
}
