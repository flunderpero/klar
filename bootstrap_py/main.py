from __future__ import annotations

import sys
from subprocess import run
from typing import Never

from . import asm_darwin_arm64, ast, ir, parser, typechecker
from . import tokenizer as token

commands = ("tokens", "ast", "types", "ir", "asm", "compile", "run")


def usage(err: str | None) -> Never:
    if err:
        print(err, file=sys.stderr)
    print("Usage: klarc <command> [... options]\n")
    print("  Commands:\n")
    print("  tokens <file>  : Tokenize and print the tokens")
    print("  ast <file>     : Parse and print the AST")
    print("  types <file>   : Type-check and print the typed AST")
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
    cur_id = 0

    def next_id() -> int:
        nonlocal cur_id
        cur_id += 1
        return cur_id

    command = sys.argv[1] if len(sys.argv) > 1 else None
    if not command:
        usage("Missing `command`")
    if command not in commands:
        usage(f"Unknown command `{command}`")
    tokens, tokenize_errors = token.tokenize(read_src(2))
    if command == "tokens":
        print("\n".join(str(x) for x in tokens))
        return
    if tokenize_errors:
        print("\nTokenize errors:\n" + "\n".join(str(x) for x in tokenize_errors))
    module, parse_errors = parser.parse(parser.Input(tokens, next_id))
    if parse_errors:
        print("\nParse errors:\n" + "\n".join(str(x) for x in parse_errors))
    if command == "ast":
        print(module)
        return
    type_env, type_errors = typechecker.typecheck(module, next_id)
    if type_errors:
        print("\nType errors:\n" + "\n".join(str(x) for x in type_errors))
    if command == "types":

        def print_typed(node: ast.Node) -> None:
            typ = type_env.node_types.get(node.id)
            typ_str = str(typ) if typ else "NOT_FOUND"
            print(node.span)
            print(node)
            print("=>", typ_str, "\n")
            ast.walk(node, print_typed)

        ast.walk(module, print_typed)
        return
    if tokenize_errors or parse_errors or type_errors:
        print("\nErrors occurred, skipping further processing")
        sys.exit(1)
    ir_ = ir.generate_ir(module, type_env)
    if command == "ir":
        print(ir_)
        return
    asm = asm_darwin_arm64.generate(ir_)
    if command == "asm":
        print(asm)
        return
    p = run(
        ["clang", "-o", "a.out", "-x", "assembler", "-"],
        input=str(asm),
        text=True,
        check=False,
        capture_output=True,
    )
    if p.returncode != 0:
        print(f"Compilation failed, clang exited with: {p.returncode}")
        if p.stdout:
            print(p.stdout)
        if p.stderr:
            print(p.stderr)
        sys.exit(p.returncode)
    if command == "compile":
        print("Compilation complete")
        return
    if command == "run":
        p = run(["./a.out"], check=False, capture_output=True, text=True)
        if p.stdout:
            print(p.stdout, end="")
        if p.stderr:
            print(p.stderr)
        if p.returncode != 0:
            print(f"\nRun failed, process exited with: {p.returncode}")
            sys.exit(p.returncode)
        return


if __name__ == "__main__":
    main()
