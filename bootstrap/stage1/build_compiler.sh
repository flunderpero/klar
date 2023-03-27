#!/bin/sh
set -e

mkdir -p build
LLVM_TARGET_TRIPLE="${LLVM_TARGET_TRIPLE:-$(llvm-config --host-target)}"

echo "Phase 1: Compile with stage0 interpreter"
echo "target triple = \"$LLVM_TARGET_TRIPLE\"" > build/compiler.ll
python ../stage0/tokenized_interpreter.py compiler.tl < compiler.tl >> build/compiler.ll
llc -O=0 -filetype=obj build/compiler.ll -o build/compiler.o
clang --target=$LLVM_TARGET_TRIPLE build/compiler.o -o build/compiler

echo "Phase 2: Compile with itself"
bash compile.sh compiler.tl build/compiler
