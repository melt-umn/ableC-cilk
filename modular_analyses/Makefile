# Top-level directory containing ableC repository and extensions folder
EXT_HOME=../../..
# All directories containing grammars that may be included
GRAMMAR_DIRS=$(wildcard $(EXT_HOME)/extensions/*/grammars) $(EXT_HOME)/ableC
# Flags passed to silver including the appropriate directories
SVFLAGS=$(addprefix -I , $(GRAMMAR_DIRS))

all: mda mwda

mda:
	touch $(wildcard determinism/*.sv)
	silver -o MDA.jar $(SVFLAGS) --clean determinism

	@echo ""
	@echo "This script runs Silver on the grammar that performs the"
	@echo "modular determinism analysis."
	@echo "A fair amount of information is displayed to the screen,"
	@echo "so look for the \"copper_mda:\" task in the output."
	@echo "There should be a line reading"
	@echo "   \"Modular determinism analysis passed.\""
	@echo "that indicates that the analysis was successful."
	@echo ""

mwda:
	touch $(wildcard well_definedness/*.sv)
	silver -o MWDA.jar $(SVFLAGS) --clean --warn-all --warn-error well_definedness
	@echo ""
	@echo "This script runs Silver on the grammar that performs the"
	@echo "modular well-definedness analysis."
	@echo "Note the use of the --warn-all flag."
	@echo "A fair amount of information is displayed to the screen,"
	@echo "so look for errors after the \"Checking for Errors.\" line."
	@echo ""

clean:
	rm -f *~ *.jar *.copperdump.html build.xml

.PHONY: all mda mwda clean
