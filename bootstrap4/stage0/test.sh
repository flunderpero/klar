#!/bin/sh
shopt -s nullglob

mkdir -p build
rm -rf build/*.out

for test_file in ../tests/stage0/*.kl; do
    test=$(basename "$test_file")
    actual="${test%.kl}.out"
    compiled="build/${test%.kl}"
    echo "$test"
    bash compile.sh "$test_file" "$compiled"
    bash -c $compiled > "build/$actual" <<< stdin_test_input
    echo -e "---\nexit code: $?" >> "build/$actual"
    if [ "$1" == "update" ]; then
        echo "Updating ${test_file%.kl}.out"
        cp "build/$actual" "${test_file%.kl}.out"
    else
        diff -u "${test_file%.kl}.out" "build/$actual" || exit 1
    fi
done
