#!/bin/sh
set -e
shopt -s nullglob

mkdir -p build
rm -rf build/*.out

for test_file in ../tests/stage{0,1,2,3,4,5,6,7,8}/*.tl; do
    if [[ "$test_file" == *"multiple_string_concatenation"* ]]; then
        continue
    fi
    test=$(basename "$test_file")
    actual="${test%.tl}.out"
    compiled="build/${test%.tl}"
    echo "$test"
    bash compile.sh "$test_file" "$compiled"
    bash -c $compiled > "build/$actual" <<< stdin_test_input
    diff -u "${test_file%.tl}.out" "build/$actual"
done
