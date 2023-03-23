#!/bin/sh
set -e

mkdir -p build

echo "Phase 1: Compile with stage0 interpreter"
python ../stage0/tokenized_interpreter.py compiler.tl < compiler.tl > build/compiler.ll
llc -O=0 -filetype=obj build/compiler.ll -o build/compiler.o
clang build/compiler.o -o build/compiler

echo "Phase 2: Compile with itself"
bash compile.sh compiler.tl build/compiler
