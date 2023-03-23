#!/bin/sh
set -e

echo "Building stage0 ..."
echo "Building stage0 done."
echo

echo "Building stage1 ..."
cd stage1
bash build_compiler.sh
echo "Building stage1 done."
echo
