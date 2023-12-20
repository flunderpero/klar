#!/bin/sh
set -e

mkdir -p build

# echo "Phase 1: Compile with stage0 compiler"
../stage0/compile.sh klarc.kl build/klarc

# echo "Phase 2: Compile with itself"
# bash compile.sh compiler.tl build/compiler
