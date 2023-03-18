#!/bin/sh

python interpreter.py parser.tl < $1 > build/a.ll
cat build/a.ll
llc -filetype=obj build/a.ll -o build/a.o
clang build/a.o -o build/a.out
