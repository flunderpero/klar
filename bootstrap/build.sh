#!/bin/sh
set -e

dir_name=$(dirname $0)

echo "Building stage0 ..."
echo "Building stage0 done."
echo

echo "Building stage1 ..."
cd $dir_name/stage1
bash build_compiler.sh
cd ..
echo "Building stage1 done."
echo

echo "Building stage2 ..."
cd stage2
bash build_compiler.sh
echo "Building stage2 done."
echo
