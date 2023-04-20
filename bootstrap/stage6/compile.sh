#!/bin/sh
set -e

build_dir="$(dirname $0)/build"
mkdir -p "$build_dir"
ulimit -s 65000

src_file="$1"
base_file=$(basename "$1")
# If the source file does not contain a function declaration we 
# surround it with a `fn main() void`. 
if ! grep -q "fn main" "$src_file"; then
    tmp_file="$build_dir/$base_file"
    echo "fn main() i32 begin" > $tmp_file
    echo "" >> $tmp_file
    cat $src_file >> $tmp_file
    echo "" >> $tmp_file
    echo "return 0" >> $tmp_file
    echo "end" >> $tmp_file
    src_file=$tmp_file
fi
ir_file="${base_file%.tl}.ll"
obj_file="${base_file%.tl}.o"
out_file="$2"
LLVM_TARGET_TRIPLE="${LLVM_TARGET_TRIPLE:-$(llvm-config --host-target)}"

echo "target triple = \"$LLVM_TARGET_TRIPLE\"" > "$build_dir/$ir_file"
bash -c "$build_dir/compiler" < "$src_file" >> "$build_dir/$ir_file"
llc -O=0 -opaque-pointers -filetype=obj "$build_dir/$ir_file" -o "$build_dir/$obj_file"
clang "$build_dir/$obj_file" -o "$out_file"
