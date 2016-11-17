// these are now in fib.orig_includes
// #include <stdlib.h>
// #include <stdio.h>
// #include <cilk.h>

// making argv a const causes a pointer problem in the generated Cilk
// code since the stored value is not const.
// Check this again when we generate more Cilk code properly, maybe this 
// will go away.

cilk int fib(int n);

cilk int main(int argc,  char **argv) {
    printf ("Testing our Cilk extension.\n");

    int n, result;

    if (argc != 2) {
        fprintf(stderr, "Usage: fib [<cilk options>] <n>\n");
        cilk_exit(1);
    }

    n = atoi((const char *) argv[1]);
    spawn result = fib(n);
    sync;

    printf("Result: %d\n", result);

    cilk_return 0;
}

cilk int fib(int n) {
    if (n < 2)
        cilk_return (n);
    else {
        int x, y;
        spawn x = fib(n - 1);
        spawn y = fib(n - 2);
        sync;
        cilk_return (x + y);
  }
}

