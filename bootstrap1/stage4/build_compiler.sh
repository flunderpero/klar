#!/bin/sh
set -e

mkdir -p build

echo "Phase 1: Compile with stage3 compiler"
../stage3/compile.sh compiler.tl build/compiler

echo "Phase 2: Compile with itself"
bash compile.sh compiler.tl build/compiler
