#!/bin/bash

# turn on option to exit on non-zero return code.
set -e
# turn on verbose option, which echos commands to stdout
set -v

# This script shows the steps in compiling an extended C program (.xc)
# down to C via an extended instance of ableC and then using GCC to
# compile the generated C code to an executable.

# For now, it works on exactly one file: fib.xc

# We assume that all #include directives are pulled out and stored in fib.orig_includes

# extract the base filename, everything before the dot (.)

filename=$1
extension="${filename##*.}"
filename_withoutpath=$(basename $filename)
basefilename="${filename_withoutpath%.*}"

# Step 1
# run java -jar ableC.jar on fib.xc, with include directive tacked on.
echo "int START_OF_XC_CODE = 1;" > fib.marker

cat fib.includes_for_xc fib.marker $basefilename.xc > $basefilename.step1.xc

java -jar ableC.jar $basefilename.step1.xc -I/usr/local/include/cilk

# TODO: decide whether steps 2 and 4 are needed

# Step 2
# extract code from $basefilename.step1.pp_out.c that came after code from the includes. 
# This is the code that ableC generated from the original code.
# This yields fib.no_includes.pp_out.c

#sed -n '/START_OF_XC_CODE/,$p' $basefilename.step1.pp_out.c > fib.no_includes.pp_out.c


# Step 4
# put fib.includes and other ext. specified includes at the top of fib.no_includes
# yeilding fib.c

#cat fib.includes_for_c fib.no_includes.pp_out.c > fib.includes.pp_out.c

# Step 5
# compile resulting C file.
#gcc -xc \
#    -I/usr/local/include/cilk \
#    -D__REENTRANT \
#    -O2 fib.includes.pp_out.c  \
#    -L/usr/local/lib -L/usr/local/lib/cilk -lcilkrt0 -lcilk -Wl,-rpath,/usr/local/lib -pthread
gcc -xc \
    -I/usr/local/include/cilk \
    -D__REENTRANT \
    -O2 $basefilename.step1.pp_out.c  \
    -o $basefilename.out \
    -L/usr/local/lib -L/usr/local/lib/cilk -lcilkrt0 -lcilk \
    -Wl,-rpath,/usr/local/lib -pthread -lm

echo "gcc return code was $?"

