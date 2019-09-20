#include <stdio.h>
#include <stdlib.h>
#include <cilk.xh>
#include <unistd.h>
#include <pthread.h>

#include <cilk.h>
#include <cilk-ableC.h>

int x;

cilk int fib(int n);
cilk void func(int n);

cilk int fib(int n) {
  if(n < 2) cilk return n;

  int x, y;
  spawn x = fib(n - 1);
  spawn y = fib(n - 2);

  sync;

  cilk return x + y;
}

cilk int test(int n) {
  int y;
  spawn y = fib(n);

  sync;

  printf("%d\n", y);

  cilk return y;
}

cilk void func(int n) {
  int y = 7;
  x += y;
}

%parallel edu:umn:cs:melt:exts:ableC:cilk=0.5

int main(int argc,  char **argv) {
  %parallelize

  x = 5;

  int res;
  spawn res = test(20);

  spawn func(5);

  sync;

  printf("fib(20) = %d\n", res);
  printf("x = %d (12)\n", x);

  return 0;
}
