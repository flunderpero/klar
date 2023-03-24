#!/bin/sh
set -e

mkdir -p build

echo "Phase 1: Compile with stage1 compiler"
../stage1/compile.sh compiler.tl build/compiler

echo "Phase 2: Compile with itself"
bash compile.sh compiler.tl build/compiler
