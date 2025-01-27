from __future__ import annotations

from dataclasses import dataclass
from subprocess import run
from typing import TYPE_CHECKING

from . import asm_darwin_arm64, ast, error, ir, lower, monomorphize, parser, typechecker
from . import tokenizer as token

if TYPE_CHECKING:
    from collections.abc import Generator


@dataclass
class TokenStep:
    tokens: list[token.Token]
    errors: list[error.Error]

    def __str__(self) -> str:
        return "\n".join(str(x) for x in self.tokens)


@dataclass
class ParseStep:
    module: ast.Module
    errors: list[error.Error]

    def __str__(self) -> str:
        return str(self.module)


@dataclass
class TypecheckStep:
    module: ast.Module
    type_env: typechecker.TypeEnv
    errors: list[error.Error]

    def __str__(self) -> str:
        lines = []

        def visit(node: ast.Node, _parent: ast.Node | None) -> None:
            typ = self.type_env.node_types.get(node.id)
            typ_str = str(typ) if typ else "NOT_FOUND"
            lines.append(str(node.span))
            lines.append(str(node))
            lines.append(f"=> {typ_str}\n")
            ast.walk(node, visit)

        ast.walk(self.module, visit)
        return "\n".join(lines)


@dataclass
class AbortStep:
    errors: list[error.Error]

    def __str__(self) -> str:
        return "\n".join(str(x) for x in self.errors)


@dataclass
class LowerStep:
    fn_specs: list[lower.FnSpec]

    def __str__(self) -> str:
        return "\n".join(str(x) for x in self.fn_specs)


@dataclass
class IRStep:
    ir: ir.IR

    def __str__(self) -> str:
        return str(self.ir)


@dataclass
class ASMStep:
    asm: str

    def __str__(self) -> str:
        return str(self.asm)


@dataclass
class CompileStep:
    returncode: int
    stdout: str
    stderr: str

    def __str__(self) -> str:
        return f"statuscode: {self.returncode}\nstdout: {self.stdout}\nstderr: {self.stderr}"


@dataclass
class RunStep:
    returncode: int
    stdout: str
    stderr: str

    def __str__(self) -> str:
        return f"statuscode: {self.returncode}\nstdout: {self.stdout}\nstderr: {self.stderr}"


CompilationStep = (
    TokenStep | ParseStep | TypecheckStep | AbortStep | LowerStep | IRStep | ASMStep | CompileStep | RunStep
)


def compile(input: token.Input, outfile: str) -> Generator[CompilationStep]:  # noqa: A001
    cur_id = 0

    def next_id() -> int:
        nonlocal cur_id
        cur_id += 1
        return cur_id

    tokens, tokenize_errors = token.tokenize(input)
    yield TokenStep(tokens, tokenize_errors)
    module, parse_errors = parser.parse(parser.Input(tokens, next_id))
    yield ParseStep(module, parse_errors)
    type_env, type_errors = typechecker.typecheck(module, next_id)
    yield TypecheckStep(module, type_env, type_errors)
    if tokenize_errors or parse_errors or type_errors:
        yield AbortStep(tokenize_errors + parse_errors + type_errors)
        return
    specs = monomorphize.monomorphize(module, type_env)
    yield LowerStep(specs)
    ir_ = ir.generate_ir(specs)
    yield IRStep(ir_)
    asm = asm_darwin_arm64.generate(ir_)
    yield ASMStep(asm)
    p = run(
        ["clang", "-o", outfile, "-g", "-x", "assembler", "-"],
        input=str(asm),
        text=True,
        check=False,
        capture_output=True,
    )
    yield CompileStep(p.returncode, p.stdout, p.stderr)
    if p.returncode != 0:
        return
    p = run([outfile], check=False, capture_output=True, text=True)
    yield RunStep(p.returncode, p.stdout, p.stderr)
