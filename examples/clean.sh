#!/bin/bash

# turn on option to exit on non-zero return code.
set -e
# turn on verbose option, which echos commands to stdout
set -v

rm -f fib.marker fib.step1.xc fib.step1.pp_out.c fib.no_includes.pp_out.c \
      fib.includes.pp_out.c fib.step1.gen_cpp a.out

