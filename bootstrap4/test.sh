#!/bin/sh
set -e

for stage in stage*; do
    echo "Running tests for $stage"
    echo
    cd "$stage"
    bash test.sh
    cd ..
    echo
done
