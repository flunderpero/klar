#!/bin/sh
set -e

mkdir -p build
rm -rf build/*.out

for test_file in ../tests/stage0/*.tl; do
    test=$(basename "$test_file")
    actual="${test%.tl}.out"
    echo "$test"
    python tokenized_interpreter.py "$test_file" > "build/$actual" <<< stdin_test_input
    diff -u "${test_file%.tl}.out" "build/$actual"
done
