#!/bin/sh
set -e
shopt -s nullglob

compiler=0
lexer=0
parser=0
the_book=0
klarfmt=0
files=0

if [ "$1" == "" ]; then
    lexer=1
    parser=1
    the_book=1
    klarfmt=1
    files=1
else
    case $1 in
        compiler)
            compiler=1
            ;;
        lexer)
            lexer=1
            ;;
        parser)
            parser=1
            ;;
        the_book)
            the_book=1
            ;;
        klarfmt)
            klarfmt=1
            ;;
        files)
            files=1
            ;;
        *)
            echo "Unknown test target: $1"
            exit 1
            ;;
    esac
fi

mkdir -p build
if [[ "$compiler" -eq 1 || "$files" -eq 1 ]]; then
    echo "Building compiler ..."
    bash build_compiler.sh
fi

if [ "$lexer" -eq 1 ]; then
    echo "Testing lexer ..."
    bun ../stage0/compiler.ts lexer.spec.kl build/lexer.spec.js --debug-errors
    build/lexer.spec.js $@
fi

if [ "$parser" -eq 1 ]; then
    echo "Testing parser ..."
    bun ../stage0/compiler.ts parser.spec.kl build/parser.spec.js --debug-errors
    build/parser.spec.js $@
fi

if [ "$klarfmt" -eq 1 ]; then
    echo "Testing klarfmt ..."
    bun ../stage0/compiler.ts klarfmt.spec.kl build/klarfmt.spec.js --debug-errors
    build/klarfmt.spec.js $@ 
fi

if [ "$the_book" -eq 1 ]; then
    echo "Testing the_book.md ..."
    bun ../stage0/compiler.ts the_book.spec.kl build/the_book.spec.js --debug-errors
    build/the_book.spec.js $@ --bail
fi

if [ "$files" -eq 1 ]; then
    for directory in . ../tests/stage0; do
        echo "Testing all source files under ${directory} ..."
        for test_file in ${directory}/*.kl; do
            if [[ "$test_file" == *"compile_error"* ]]; then
                continue
            fi
            test=$(basename "$test_file")
            compiled="build/${test%.kl}"
            echo "$test"
            bash compile.sh "$test_file" "$compiled"
        done
    done
fi
