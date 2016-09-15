#!/bin/bash

# turn off option to exit on non-zero return code.
set +e

./the_tests.sh

if [ "$?" == "0" ]; then
    echo ""
    echo "Success!"
    echo ""
else
    echo ""
    echo "Tests failed!"
    echo ""
fi
