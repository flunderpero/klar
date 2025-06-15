
precommit:
    just bootstrap precommit

[positional-arguments]
bootstrap *args:
    @just -f bootstrap_py/justfile -- "$@"
