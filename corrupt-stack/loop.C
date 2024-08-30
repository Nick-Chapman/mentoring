
#include "stdio.h"

long counter;

void f() {
  long thing;
  printf("%ld\n",++counter);
  if (counter < 10) (&thing)[2] -= 5;
}

int main() {
  f();
}
