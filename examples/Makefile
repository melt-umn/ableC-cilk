# Top-level directory containing ableC repository and extensions folder
ABLEC_HOME=../../..

# All directories containing grammars that may be included
GRAMMAR_DIRS=$(ABLEC_HOME)/ableC $(ABLEC_HOME)/extensions/ableC-cilk/grammars

# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find $(GRAMMAR_DIRS) -name *.sv -print0 | xargs -0)

EXAMPLES = 

all: ableC.jar examples

ableC.jar: $(GRAMMAR_SOURCES)
	touch artifact/Artifact.sv
	silver -o $@ -I $(ABLEC_HOME)/ableC \
		     -I $(ABLEC_HOME)/extensions/ableC-cilk/grammars \
		     artifact

fib_new.c:	fib_new.xc
	java -jar ableC.jar fib_new.xc -I/usr/local/include/cilk

fib_new.out:	fib_new.c
	gcc -xc -I/usr/local/include/cilk -D__REENTRANT -O2 \
		fib_new.c -o fib_new.out \
		-L/usr/local/lib -L/usr/local/lib/cilk -lcilkrt0 -lcilk \
		-Wl,-rpath,/usr/local/lib -pthread -lm





examples: cilksort.out fft.out fib.out \
	matmul.out nfib.out \
	test-locks.out test-void-func.out velin.out \
	# TODO: bucket.out heat.out knapsack.out plu.out


%.out : %.xc ableC.jar
	./compile.sh $<

clean:
	rm -f *~ *.i *.c *.o *.out *.test *.jar *.copperdump.html build.xml

.PHONY: all clean
