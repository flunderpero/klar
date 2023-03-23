#!/bin/sh
set -e

mkdir -p build
rm -rf build/*.out

for test_file in ../tests/*.tl; do
    test=$(basename "$test_file")
    actual="${test%.tl}.out"
    compiled="build/${test%.tl}"
    echo "$test"
    bash compile.sh "$test_file" "$compiled"
    bash -c $compiled > "build/$actual" <<< stdin_test_input
    diff -u "${test_file%.tl}.out" "build/$actual"
done
