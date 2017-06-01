
all: examples analyses test

build:
	@cd examples && make ableC.jar

examples:
	@cd examples && make examples

analyses: mda mwda

mda:
	@cd modular_analyses && make mda

mwda:
	@cd modular_analyses && make mwda

test:
	@cd test && make -ij

clean:
	rm -f *~ 
	@cd examples && make clean
	@cd modular_analyses && make clean
	@cd test && make clean

.PHONY: all examples analyses mda mwda test clean
