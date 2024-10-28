# Klar - Programming Should Be Fun

Klar is a general purpose, safe, and fast compiled language. It strives to be simple yet elegant and
hackable.

Klar is developed from scratch without the use of external libraries. The only exception is using
an assembler in the bootstrap process. Everything else - the tokenizer, parser, type-checker,
immediate representation, and assembly code generation is written by hand. Why? Because it's fun
and there is a lot to learn.

The targeted architectures will be arm64, amd64, and WebAssembly.

## Bootstrap

The bootstrap compiler (`/bootstrap`) is written in Go. Each commit follows a story from start to
finish, so you should be able to learn everything about each aspect of a compiler by following the
commits.

Usage:

    go run bootstrap/*.go <command> <file>

    # Build and run examples/hello-world.kl
    go run bootstrap/*.go run examples/hello-world.kl
