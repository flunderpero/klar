# ruff: noqa: T201
from __future__ import annotations

import sys
from typing import Never

from . import compiler
from . import tokenizer as token

commands = ("tokens", "ast", "types", "lower", "ir", "asm", "compile", "run")


def usage(err: str | None) -> Never:
    if err:
        print(err, file=sys.stderr)
    print("Usage: klarc <command> [... options]\n")
    print("  Commands:\n")
    print("  tokens <file>  : Tokenize and print the tokens")
    print("  ast <file>     : Parse and print the AST")
    print("  types <file>   : Type-check and print the typed AST")
    print("  lower <file>   : Lower the AST")
    print("  ir <file>      : Generate and print the IR")
    print("  asm <file>     : Generate and print the assembly code")
    print("  compile <file> : Compile the source to `a.out`")
    print("  run <file>     : Compile and run the source")
    sys.exit(1 if err else 0)


def read_src(argv_pos: int) -> token.Input:
    file = sys.argv[argv_pos] if len(sys.argv) > argv_pos else None
    if not file:
        usage("Missing `file`")
    try:
        src = open(file).read()
        return token.Input(file, src)
    except BaseException as e:
        print(f"Unable to read file `{file}`: {e}")
        sys.exit(1)


def main() -> None:
    command = sys.argv[1] if len(sys.argv) > 1 else None
    if not command:
        usage("Missing `command`")
    if command not in commands:
        usage(f"Unknown command `{command}`")
    for step in compiler.compile(read_src(2), "./a.out"):
        match step:
            case compiler.TokenStep():
                if step.errors:
                    print("\nTokenize errors:\n" + "\n".join(str(x) for x in step.errors))
                if command == "tokens":
                    print(step)
                    break
            case compiler.ParseStep():
                if step.errors:
                    print("\nParse errors:\n" + "\n".join(str(x) for x in step.errors))
                if command == "ast":
                    print(step)
                    break
            case compiler.TypecheckStep():
                if step.errors:
                    print("\nType errors:\n" + "\n".join(str(x) for x in step.errors))
                if command == "types":
                    print(step)
                    break
            case compiler.AbortStep():
                print("\nErrors occurred, skipping further processing")
                sys.exit(1)
            case compiler.LowerStep():
                if command == "lower":
                    print(step)
                    break
            case compiler.IRStep():
                if command == "ir":
                    print(step)
                    break
            case compiler.ASMStep():
                if command == "asm":
                    print(step)
                    break
            case compiler.CompileStep():
                if step.returncode != 0:
                    print(f"Compilation (clang) failed with status {step.returncode}")
                    print(f"stdout:\n{step.stdout}\nstderr:\n{step.stderr}")
                    sys.exit(step.returncode)
                if command == "compile":
                    break
            case compiler.RunStep():
                if step.stdout:
                    print(step.stdout, end="")
                if step.stderr:
                    print(step.stderr)
                if step.returncode != 0:
                    print(f"\nRun failed, process exited with: {step.returncode}")
                    sys.exit(step.returncode)
                break
            case _:
                raise AssertionError(f"Unknown step: {step}")


if __name__ == "__main__":
    main()
