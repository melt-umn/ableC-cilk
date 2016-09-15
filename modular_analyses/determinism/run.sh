#!/bin/bash

# turn on verbose option, which echos commands to stdout
set -v

silver -I ../../.. -I ../../../../ableC -o MDA.jar --clean $@ \
       edu:umn:cs:melt:exts:ableC:cilk:modular_analyses:determinism

# This script runs Silver on the grammar that performs the modular
# determinism analysis.  A fair amount of information is displayed to
# the screen, so look for the "copper_mda:" task in the output.  There
# should be a line reading "Modular determinism analysis passed." that
# indicates that the analysis was successful.

