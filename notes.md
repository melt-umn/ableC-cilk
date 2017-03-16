These notes are intended to document difficulties encountered and differences between the original MIT Cilk and the Cilk ableC extension.

## Changes in syntax

TODO: explain why these changes are required

### spawn
`x = spawn foo();` must be replaced with `spawn x = foo();`.

### return
`return` in a Cilk function must be replaced with `cilk_return`. This is also true of implicit returns from functions that return void (currently the Cilk extension appends a call to `cilk_return;` to the body of any Cilk function returning void, just in case the user forgets to do this, but it is not clear this is the right thing to do.)

### exit
`exit` in a Cilk function must be replaced with `cilk_exit`.

## Difficulties

### Avoiding rewriting of terms in Cilk function bodies
TODO: write this subsection

### Passing around syncCount and scopeCount without introducing new dependencies on flow types
TODO: write this subsection

## Bugs / TODO

### Arrays
See [Issue #1](https://github.umn.edu/melt/edu.umn.cs.melt.exts.ableC.cilk/issues/1).

Arrays are not currently handled correctly. `int main(int argc, char *argv[])` must be replaced with `int main(int argc, char **argv)`, for example. Similarly, any arrays in Cilk function bodies must be replaced by pointers, e.g. replace `int x[5];` with `int *x = malloc(5 * sizeof(*x));`.

This issue has simply not been looked into closely yet; it might or might not be the case that there is a fundamental problem with the extension here, but there is currently no reason to think that this can't be solved.

### Variable Shadowing
See [Issue #2](https://github.umn.edu/melt/edu.umn.cs.melt.exts.ableC.cilk/issues/2).

Variable shadowing is not allowed in Cilk functions. This is at least partially a consequence of our avoidance of term rewriting in the body of Cilk functions.

Note: this also seems to be an issue with the original MIT Cilk when inlets are used (but not in non-inlet Cilk functions).

### Non-identifier lhs of spawn, non-'=' AssignOp with spawn
See [Issue #3](https://github.umn.edu/melt/edu.umn.cs.melt.exts.ableC.cilk/issues/3).

Statements like `spawn x[2] = foo();` or `spawn x += foo();` don't work.

This issue has simply not been looked into closely yet; it might or might not be the case that there is a fundamental problem with the extension here, but there is currently no reason to think that this can't be solved.

### Inlets
Support for inlets has not been developed yet. Inlets are an advanced Cilk feature that allow the result of a spawn to be passed to a function, rather than simply used in an assignment.

### Abort
Support for aborting spawned threads has not been developed yet.


