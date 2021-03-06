#include <stdio.h>
#include <stdlib.h>
#include <cilk.xh>

cilk int fib(int n);

cilk int main(int argc,  char **argv) {
    int n, result;

//    if (argc != 2) {
//        fprintf(stderr, "Usage: fib [<cilk options>] <n>\n");
//        exit(1);
//    }
//
//    n = atoi((const char *) argv[1]);
    n = 30;
    spawn result = fib(n);
    sync;

    printf("Result: %d\n", result);

    cilk return 0;
}

cilk int fib(int n) {
    if (n < 2)
        cilk return (n);
    else {
        int x, y;
        spawn x = fib(n - 1);
        spawn y = fib(n - 2);
        sync;
        cilk return (x + y);
  }
}

