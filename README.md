**This extension is not currently maintained**

# ableC-cilk

The Cilk parallel programming constructs implemented as a language
extension to ableC.

Run
```
make analyses
```
to verify that the modular analyses pass.

Then run
```
make examples
```
to verify them.

The generated `fig.out` in examples can be run as
```
./fib.out -nproc 4 33
```
to calculate the 33rd Fibonacci number using 4 processors.

