# To build and/or test the extension, run one of the following commands:
#
# `make`: build the artifact and run all tests
#
# `make build`: build the artifact
#
# `make examples`: compile and run the example uses of the extension
#
# `make analyses`: run the modular analyses that provide strong composability
#                  guarantees
#
# `make mda`: run the modular determinism analysis that ensures that the
#             composed specification of the lexical and context-free syntax is
#             free of ambiguities
#
# `make mwda`: run the modular well-definedness analysis that ensures that the
#              composed attribute grammar is well-defined and thus the semantic
#              analysis and code generation phases will complete successfully
#
# `make test`: run the extension's test suite
#
# note: the modular analyses and tests will not be rerun if no changes to the
#       source have been made. To force the tests to run, use make's -B option,
#       e.g. `make -B analyses`, `make -B mwda`, etc.
#

# Path from current directory to top level ableC repository
ABLEC_BASE?=../../ableC
# Path from current directory to top level extensions directory
EXTS_BASE?=../../extensions

MAKEOVERRIDES=ABLEC_BASE=$(abspath $(ABLEC_BASE)) EXTS_BASE=$(abspath $(EXTS_BASE))

all: examples analyses test

build:
	cd examples && $(MAKE) ableC.jar

examples:
	cd examples && $(MAKE) -j

analyses: mda mwda

mda:
	cd modular_analyses && $(MAKE) mda

mwda:
	cd modular_analyses && $(MAKE) mwda

test:
	cd test && $(MAKE) -kj

clean:
	rm -f *~ 
	cd examples && $(MAKE) clean
	cd modular_analyses && $(MAKE) clean
	cd test && $(MAKE) clean

.PHONY: all build examples analyses mda mwda test clean
.NOTPARALLEL: # Avoid running multiple Silver builds in parallel
