#include <stdio.h>
#include <stdlib.h>
#include <cilk.xh>

cilk double fold(double (*op)(double, double), double init, size_t len, double *data);
cilk double fold(double (*op)(double, double), double init, size_t len, double *data) {
  if (len == 0) {
    cilk return init;
  } else if (len == 1) {
    cilk return data[0];
  } else {
    double res1, res2;
    spawn res1 = fold(op, init, len / 2, data);
    spawn res2 = fold(op, init, len - (len / 2), data + len / 2);
    sync;
    cilk return op(res1, res2);
  }
}

double add(double a, double b) {
  return a + b;
}

cilk int main(int argc,  char **argv) {
  if (argc != 2) {
    fprintf(stderr, "Usage: fold [<cilk options>] <n>\n");
    exit(1);
  }

  int size = atoi(argv[1]);
    
  double *data = malloc(size * sizeof(double));
  for (size_t i = 0; i < size; i++) {
    data[i] = i + 1;
  }
  
  printf("Folding\n");
  double res;
  spawn res = fold(add, 1, size, data);
  sync;
  printf("Result: %f\n", res);
}

