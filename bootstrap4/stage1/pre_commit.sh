#!/bin/sh
set -e
shopt -s nullglob

echo "Formatting code ..."
bash compile.sh klarfmt.kl build/klarfmt.js
for file in *.kl; do
    echo "Formatting $file ..."
    bun build/klarfmt.js $file -w
done

echo "Running tests ..."
bash test.sh 
