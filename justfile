precommit:
    just bootstrap precommit

bootstrap *args:
    just -f bootstrap_py/justfile {{args}}
