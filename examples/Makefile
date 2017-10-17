# Top-level directory containing ableC repository and extensions folder
ABLEC_HOME=../../..

# All directories containing grammars that may be included
GRAMMAR_DIRS=$(ABLEC_HOME)/ableC $(ABLEC_HOME)/extensions/ableC-cilk/grammars artifact

# All silver files in included grammars, to be included as dependancies
GRAMMAR_SOURCES=$(shell find $(GRAMMAR_DIRS) -name *.sv -print0 | xargs -0)

EXAMPLES = 

all: ableC.jar examples

ableC.jar: $(GRAMMAR_SOURCES)
	touch artifact/Artifact.sv
	silver -o $@ $(SVFLAGS) -I $(ABLEC_HOME)/ableC \
		     -I $(ABLEC_HOME)/extensions/ableC-cilk/grammars \
		     artifact

fib.c:	fib.xc ableC.jar
	java -Xss6M -jar ableC.jar fib.xc -I/usr/local/include/cilk -I../include

fib.out:	fib.c
	gcc -xc -I/usr/local/include/cilk -I../include -D__REENTRANT -O2 \
		fib.c -o fib.out \
		-L/usr/local/lib -L/usr/local/lib/cilk -lcilkrt0 -lcilk \
		-Wl,-rpath,/usr/local/lib -pthread -lm

examples:	fib.out

clean:
	rm -f *~ *.i *.c *.o *.out *.test *.jar *.copperdump.html build.xml ableC.jar

.PHONY: all clean
