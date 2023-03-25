#!/bin/sh
set -e

build_dir="$(dirname $0)/build"
mkdir -p "$build_dir"
ulimit -s 65000

src_file="$1"
base_file=$(basename "$1")
ir_file="${base_file%.tl}.ll"
obj_file="${base_file%.tl}.o"
out_file="$2"

bash -c "$build_dir/compiler" < "$src_file" > "$build_dir/$ir_file"
llc -O=0 -filetype=obj "$build_dir/$ir_file" -o "$build_dir/$obj_file"
clang "$build_dir/$obj_file" -o "$out_file"
