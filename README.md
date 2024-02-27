# The Klar Language

I am programming for 35 years now and 34 years of those I wanted to write my own
programming language.

This is all work-in-progress and definitely not ready for anything.

Ignore everything outside of `bootstrap4`.

## Self-Hosting Compiler

There have been some attempts to build a self-hosting compiler (`bootstrap1`
to `bootstrap4`) with some funny approaches. I want to write a minimal stage0
compiler to get to the self-hosting compiler as quickly as possible. 

The current version is `bootstrap4`. The stage0-compiler is written in JavaScript
and uses [Bun](https://bun.sh/) to compile itself. The output is also JavaScript.
Why JavaScript? Quick and dirty prototyping.

## Play With It

```sh
# Clone the repository.
$ git clone https://github.com/flunderpero/klar.git
$ cd klar/bootstrap4/stage0

# (Optional) Install Bun.
$ curl -fsSL https://bun.sh/install | bash 

# Compile and run some Klar code.
$ bash compile.sh ../tests/stage0/strings.kl strings.js
$ bun strings.js
```

## Stage1 (bootstrap4/stage1)

The stage1-compiler is written in Klar. Currently the lexer and most of the parser
is implemented. I am currently unsure about the compilation target. Most likely it
will be LLVM IR like in other bootstrapping attempts. 

## Syntax Highlighting

If you are fortunate enough to use [Neovim](https://neovim.io/) you can use the
[Tree-sitter parser for Klar](https://github.com/flunderpero/tree-sitter-klar).

