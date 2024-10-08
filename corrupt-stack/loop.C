
//#define DEBUG

#include "stdio.h"

long counter = 0; // to make you happy!

void my_function() {
  long thing = 42; // pick a value so we can see it in the prints
  printf("%ld\n",++counter); // increment then print. so we first see a 1.

#ifdef DEBUG
  // Lets look up the stack to try and find the caller's return address...
  // (Remember, stacks grow downwards!)
  printf("(&thing)[0]=%p\n", (void*)(&thing)[0]); // I see 0x2a (the hex for thing=42)
  printf("(&thing)[1]=%p\n", (void*)(&thing)[1]); // dont know what's here
  printf("(&thing)[2]=%p\n", (void*)(&thing)[2]); // Here I see L0, printed below
  printf("(&thing)[3]=%p\n", (void*)(&thing)[3]); // more garbage
#else
  if (counter < 10) (&thing)[2] -= 5; // reinstate when we figure out where the return addree is!
#endif
}

int main() {
#ifdef DEBUG
 L0: // This is label. Normally the only thing you can do is goto a label.
  my_function();
 L1:
  printf("L0=%p\n",&&L0); // This && is jinky gcc syntax to take the address of a label
  printf("L1=%p\n",&&L1);
  printf("diff=%ld\n",(long)&&L1 - (long)&&L0); // I see 5 here. Means code for "f();" is 5 words long
#else
  my_function();
#endif
}

/* Sample run.. everytime the addresses are slightly different.

nic@Mosh:~/code/mentoring/corrupt-stack$ make
g++ loop.C -o loop.exe
bash -c './loop.exe'
1
(&thing)[0]=0x2a
(&thing)[1]=0x7fff9208e4a0
(&thing)[2]=0x55eb665ed806
(&thing)[3]=0x1
L0=0x55eb665ed801
L1=0x55eb665ed806
diff=5

*/
