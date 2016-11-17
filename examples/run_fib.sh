#!/bin/bash

# turn on option to exit on non-zero return code.
set -e
# turn on verbose option, which echos commands to stdout
set -v

# Use 6 processors to compute fib of 30
#./a.out -nproc 6 30

# slow clone not implemented, run with 1 processor for now
./a.out -nproc 1 30


