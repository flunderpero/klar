#!/bin/sh
set -e
shopt -s nullglob

echo "Building compiler ..."
mkdir -p build
bash build_compiler.sh

echo "Testing lexer ..."
bun ../stage0/compiler.ts lexer.kl build/lexer
build/lexer

echo "Testing all source files under ../tests/stage0 ..."
mkdir -p build
for test_file in ../tests/stage0/*.kl; do
    test=$(basename "$test_file")
    compiled="build/${test%.kl}"
    echo "$test"
    bash compile.sh "$test_file" "$compiled"
done
