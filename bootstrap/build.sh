#!/bin/sh
set -e

dir_name=$(dirname $0)

for stage in $dir_name/stage*; do
    echo "Building $stage ..."
    cd $dir_name/$stage
    bash build_compiler.sh
    cd ..
    echo "Building $stage done."
    echo
done
