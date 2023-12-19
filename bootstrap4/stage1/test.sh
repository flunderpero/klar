#!/bin/sh
set -e

bun ../stage0/compiler.ts lexer.kl build/lexer
build/lexer
