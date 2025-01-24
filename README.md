# Klar - Programming Should Be Fun

Klar is a general purpose, safe, and fast compiled language. It strives to be simple yet elegant and
hackable.

Klar is developed from scratch without the use of external libraries. The only exception is using
an assembler in the bootstrap process. Everything else - the tokenizer, parser, type-checker,
immediate representation, and assembly code generation is written by hand. Why? Because it's fun
and there is a lot to learn.

The target architectures will be arm64, amd64, and WebAssembly. For now, arm64 on MacOS is
supported.

## Documentation

Look at `./bootstrap_py/klar_the_book.md` for a living documentation.

## Bootstrap

The bootstrap compiler (`./bootstrap_py`) is written in Python. Each commit follows a story from start to
finish, so you should be able to learn everything about each aspect of a compiler by following the
commits.

Usage:

```bash

python -m bootstrap_py.main

# Build and run examples/hello-world.kl:
python -m bootstrap_py.main run examples/hello_world.kl

# Run all tests contained in `./bootstrap_py/klar_the_book.md`:
python -m bootstrap_py.md_tests bootstrap_py/klar_the_book.md

# Or use the justfile under `./bootstrap_py/justfile`:
just bootstrap test

```

<details>
    <summary>A previous attempt</summary>

A previous attempt of the bootstrap compiler is written in Go and can be found in `/bootstrap`.

Usage:

```bash

go run bootstrap/*.go <command> <file>

# Build and run examples/hello-world.kl
go run bootstrap/*.go run examples/hello-world.kl

```

</details>
