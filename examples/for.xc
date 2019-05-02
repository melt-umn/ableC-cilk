#include <stdio.h>
#include <stdlib.h>
#include <cilk.xh>
#include <cilk-ableC.h>

cilk int f(int n)
{
    printf("%d\n", n);
    cilk return 0;
}

cilk int main(int argc, char **argv) {
    cilk for (int i=0; i < 10; ++i) {
        f(i);
    }

    sync;

    cilk return 0;
}

