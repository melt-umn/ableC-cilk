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

void setup_thread_system() {
  init_thread_system();

  int retVal = atexit(destroy_thread_system);
  if(retVal != 0) {
    fprintf(stderr, "Error in atexit setup from setup_thread_system\n");
    exit(-1);
  }
}

int main(int argc,  char **argv) {
  setup_thread_system();
  init_cilk_ableC(2);

  x = 5;

  //int _cilk_join_counter = 0;
  //pthread_mutex_t _cilk_join_lock = PTHREAD_MUTEX_INITIALIZER;
  //pthread_cond_t _cilk_join_cv = PTHREAD_COND_INITIALIZER;

  int res;
  spawn res = test(20);

  spawn func(5);

  sync;

  printf("fib(20) = %d\n", res);
  printf("x = %d (12)\n", x);

  return 0;
}
