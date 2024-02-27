#!/bin/sh
set -e
shopt -s nullglob

echo "Building compiler ..."
mkdir -p build
bash build_compiler.sh

echo "Testing specs ..."
bun ../stage0/compiler.ts lexer.spec.kl build/lexer.spec.js --debug-errors
build/lexer.spec.js $@

bun ../stage0/compiler.ts parser.spec.kl build/parser.spec.js --debug-errors
build/parser.spec.js $@

echo "Testing the_book.md ..."
bun ../stage0/compiler.ts the_book.spec.kl build/the_book.spec.js --debug-errors
build/the_book.spec.js $@ --bail

echo "Testing all source files under ../tests/stage0 ..."
for test_file in ../tests/stage0/*.kl; do
    if [[ "$test_file" == *"compile_error"* ]]; then
        continue
    fi
    test=$(basename "$test_file")
    compiled="build/${test%.kl}"
    echo "$test"
    bash compile.sh "$test_file" "$compiled"
done
